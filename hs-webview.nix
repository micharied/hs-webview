{ mkDerivation, base, gtk3, lib, scotty, text, webkitgtk_4_1, src ? ./. }:
mkDerivation {
  pname = "hs-webview";
  version = "0.1.0.0";
  src = src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryPkgconfigDepends = [ gtk3 webkitgtk_4_1 ];
  executableHaskellDepends = [ base scotty text ];
  executablePkgconfigDepends = [ gtk3 webkitgtk_4_1 ];
  homepage = "https://github.com/micharied/hs-webview";
  description = "Minimal Haskell bindings for the webview library";
  license = lib.licenses.mit;
  __onlyPropagateKnownPkgConfigModules = true;
  mainProgram = "demo";
}
