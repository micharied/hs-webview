{
  description = "hs-webview - Haskell bindings for webview";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    webview = {
      url = "github:micharied/webview?ref=fix-load-frameworks";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, webview }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      mkSyncWebviewCore = pkgs:
        pkgs.writeShellApplication {
          name = "sync-webview-core";
          text = ''
            set -euo pipefail
            dest="''${1:-}"
            if [ -z "''${dest}" ]; then
              dest="$PWD/cbits"
            fi
            rm -rf "''${dest}"
            mkdir -p "''${dest}/include"
            cp ${webview}/core/src/webview.cc "''${dest}/webview.cc"
            cp -R ${webview}/core/include/. "''${dest}/include/"
            chmod -R u+w "''${dest}"
          '';
        };
      platformLibsFor = pkgs:
        if pkgs.stdenv.isDarwin then
          [ pkgs.apple-sdk ]
        else
          (with pkgs; [ gtk3 webkitgtk_4_1 pkgs.xorg.libXtst pkgs.xorg.libX11]);
      hsWebviewOverlay = final: prev:
        let
          syncWebviewCore = mkSyncWebviewCore final;
          hsWebviewSrc = final.runCommand "hs-webview-src" { } ''
            cp -r ${./.} $out
            chmod -R u+w $out
            cd $out
            ${syncWebviewCore}/bin/sync-webview-core cbits
          '';
          platformLibs = platformLibsFor final;
        in
        {
          haskellPackages = prev.haskellPackages.extend (hself: hsuper: {
            hs-webview =
              (hself.callPackage ./hs-webview.nix { src = hsWebviewSrc; }).overrideAttrs
              (oldAttrs: {
                dontWrapQtApps = true;
                buildInputs = oldAttrs.buildInputs ++ platformLibs;
              });
          });
        };
      overlays = [ hsWebviewOverlay ];
    in
    flake-utils.lib.eachSystem systems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = overlays;
        };
        syncWebviewCore = mkSyncWebviewCore pkgs;
        platformLibs = platformLibsFor pkgs;
        hsWebview = pkgs.haskellPackages.hs-webview;
      in
      {
        packages.default = hsWebview;

        apps.sync-webview = {
          type = "app";
          program = "${syncWebviewCore}/bin/sync-webview-core";
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.hs-webview ];
          buildInputs = [ pkgs.glib pkgs.cabal-install pkgs.pkg-config ]
            ++ (with pkgs.haskellPackages; [ haskell-language-server ghcid hlint hoogle fourmolu ]);
          nativeBuildInputs = platformLibs ++ [ pkgs.pkg-config syncWebviewCore ];
          shellHook = ''
            # Keep a sane coreutils/xargs ahead of bootstrap-tools.
            export PATH=${pkgs.findutils}/bin:${pkgs.coreutils}/bin:$(printf '%s\n' "$PATH" | tr : '\n' | sed '/bootstrap-tools/d' | paste -sd:)

            # Avoid the giant NIX_CFLAGS_* that shuffles glibc ahead of libstdc++;
            # let pkg-config/GHC supply include paths.
            unset NIX_CFLAGS_COMPILE NIX_CFLAGS_LINK

            if [ ! -f cbits/webview.cc ]; then
              echo "Populating cbits/ from webview input..."
              sync-webview-core cbits
            fi
          '';
        };
      }) // {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;
    };
}
