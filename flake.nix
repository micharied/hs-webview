{
  description = "hs-webview - Haskell bindings for webview";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    webview = {
      url = "github:micharied/webview?ref=develop";
      flake = false;
    };
    hs-bindgen = {
      url = "github:well-typed/hs-bindgen";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      webview,
      hs-bindgen,
    }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      mkSyncWebviewCore =
        pkgs:
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
      hsWebviewOverlay =
        final: prev:
        let
          syncWebviewCore = mkSyncWebviewCore final;
          hsWebviewSrc = final.runCommand "hs-webview-src" { } ''
            cp -r ${./.} $out
            chmod -R u+w $out
            cd $out
            ${syncWebviewCore}/bin/sync-webview-core cbits
          '';
        in
        {
          haskellPackages = prev.haskellPackages.extend (
            hself: hsuper: {
              hs-webview =
                final.haskell.lib.compose.overrideCabal
                  (drv: {
                    __onlyPropagateKnownPkgConfigModules = true;
                  })
                  (
                    (hself.callCabal2nix "hs-webview" hsWebviewSrc { }).overrideAttrs (oldAttrs: {
                      dontWrapQtApps = true;
                      buildInputs = oldAttrs.buildInputs ++ final.lib.optional final.stdenv.isDarwin final.apple-sdk;
                    })
                  );
            }
          );
        };
      overlays = [
        hs-bindgen.overlays.default
        hsWebviewOverlay
      ];
      mkRegenBindings =
        pkgs:
        pkgs.writeShellApplication {
          name = "regen-bindings";
          runtimeInputs = [
            pkgs.hs-bindgen-cli
            pkgs.coreutils
          ];
          text = ''
            set -euo pipefail
            root="''${1:-$PWD}"
            cd "''${root}"
            rm -rf src/WebView/Raw src/WebView/Raw.hs
            hs-bindgen-cli preprocess \
              --unique-id io.github.micharied.hs-webview \
              --hs-output-dir src \
              --create-output-dirs \
              --overwrite-files \
              --module WebView.Raw \
              --select-by-header-path 'webview/.*\.h' \
              --enable-program-slicing \
              --select-except-deprecated \
              -I "''${root}/cbits/include" \
              webview/webview.h
            # We do not expose the FunPtr module; drop it to keep the cabal
            # surface small.
            rm -f src/WebView/Raw/FunPtr.hs
          '';
        };
      mkCheckBindings =
        pkgs: regenBindings:
        pkgs.writeShellApplication {
          name = "check-bindings";
          runtimeInputs = [
            regenBindings
            pkgs.coreutils
            pkgs.diffutils
          ];
          text = ''
            set -euo pipefail
            root="''${1:-$PWD}"
            if [ ! -f "''${root}/cbits/include/webview/webview.h" ]; then
              echo "cbits/ is empty; run 'nix run .#sync-webview' first" >&2
              exit 1
            fi
            tmp=$(mktemp -d)
            trap 'rm -rf "''${tmp}"' EXIT
            ln -s "''${root}/cbits" "''${tmp}/cbits"
            mkdir -p "''${tmp}/src"
            regen-bindings "''${tmp}"
            diff -ru "''${root}/src/WebView/Raw"    "''${tmp}/src/WebView/Raw"
            diff -u  "''${root}/src/WebView/Raw.hs" "''${tmp}/src/WebView/Raw.hs"
            echo "generated bindings are up to date"
          '';
        };
    in
    flake-utils.lib.eachSystem systems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = overlays;
        };
        syncWebviewCore = mkSyncWebviewCore pkgs;
        regenBindings = mkRegenBindings pkgs;
        checkBindings = mkCheckBindings pkgs regenBindings;
        hsWebview = pkgs.haskellPackages.hs-webview;
      in
      {
        packages.default = hsWebview;
        legacyPackages = pkgs;

        apps.sync-webview = {
          type = "app";
          program = "${syncWebviewCore}/bin/sync-webview-core";
        };

        apps.regen-bindings = {
          type = "app";
          program = "${regenBindings}/bin/regen-bindings";
        };

        apps.check-bindings = {
          type = "app";
          program = "${checkBindings}/bin/check-bindings";
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.hs-webview ];
          buildInputs = [
            pkgs.glib
            pkgs.cabal-install
            pkgs.pkg-config
          ]
          ++ (with pkgs.haskellPackages; [
            haskell-language-server
            ghcid
            hlint
            hoogle
            fourmolu
          ]);
          nativeBuildInputs = [
            pkgs.pkg-config
            syncWebviewCore
            regenBindings
            pkgs.hs-bindgen-cli
            pkgs.hsBindgenHook
          ];
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
      }
    )
    // {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;
    };
}
