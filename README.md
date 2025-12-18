# hs-webview

Minimal Haskell bindings for the official webview C API. The library builds the
upstream `core/src/webview.cc` translation unit directly and exposes a tiny API
around window creation, lifecycle management, navigation and HTML rendering.

## Prerequisites

- Build is driven by Nix, so you only need a working Nix installation.
- The flake automatically vendors the upstream `webview/core` sources into
  `cbits/` via the `webview` flake input, so the project can live outside of the
  upstream repository.
- The flake pulls in GTK/WebKit dependencies on Linux and Cocoa/WebKit
  frameworks on macOS automatically.

## Quick start

```bash
cd hs-webview
nix develop
cabal run demo
```

`cabal run demo` compiles the library and launches the sample program in
`app/Main.hs`, which opens a WebView window rendering inline HTML.

## Updating vendored C sources

Whenever you need to refresh the embedded `core/src/webview.cc` + headers, run
the helper app:

```bash
nix run .#sync-webview
```

The development shell also keeps `cbits/` populated automatically. This is the
same mechanism that `callCabal2nix` uses during Nix builds, ensuring the
packaged tarball always contains the correct C sources.

## Next steps

- Expand the Haskell API surface as needed (callbacks, dispatch, bindings to
  more of the C API)
- Add CI builds for Linux and macOS using the provided flake outputs
- Publish the package to Hackage once the API stabilises
