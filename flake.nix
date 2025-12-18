{
  description = "hs-webview - Haskell bindings for webview";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    webview = {
      url = "github:webview/webview";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, webview }:
    flake-utils.lib.eachSystem [
      "aarch64-darwin"
      "x86_64-darwin"
      "aarch64-linux"
      "x86_64-linux"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        syncWebviewCore = pkgs.writeShellApplication {
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
        hsWebviewSrc = pkgs.runCommand "hs-webview-src" { } ''
          cp -r ${./.} $out
          chmod -R u+w $out
          cd $out
          ${syncWebviewCore}/bin/sync-webview-core cbits
        '';
        platformLibs =
          if pkgs.stdenv.isDarwin then
            [ pkgs.apple-sdk ]
          else
            (with pkgs; [ gtk3 webkitgtk_4_1 ]);
        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            hs-webview =
              (prev.callCabal2nix "hs-webview" hsWebviewSrc { }).overrideAttrs (old: {
                buildInputs = (old.buildInputs or [ ]) ++ platformLibs;
              });
          };
        };
        hsWebview = haskellPackages.hs-webview;
      in
      {
        packages.default = hsWebview;

        apps.sync-webview = {
          type = "app";
          program = "${syncWebviewCore}/bin/sync-webview-core";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.hs-webview ];
          buildInputs = platformLibs
            ++ [ pkgs.cabal-install pkgs.pkg-config ]
            ++ (with haskellPackages; [ haskell-language-server ghcid hlint hoogle fourmolu ]);
          nativeBuildInputs = [ pkgs.pkg-config syncWebviewCore ];
          shellHook = ''
            if [ ! -f cbits/webview.cc ]; then
              echo "Populating cbits/ from webview input..."
              sync-webview-core cbits
            fi
          '';
        };
      });
}
