{ mkDerivation, base, lib, scotty, text, platformPkgconfigDeps ? [ ], src ? ./. }:
mkDerivation {
  pname = "hs-webview";
  version = "0.1.0.0";
  src = src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryPkgconfigDepends = platformPkgconfigDeps;
  executableHaskellDepends = [ base scotty text ];
  executablePkgconfigDepends = platformPkgconfigDeps;
  homepage = "https://github.com/micharied/hs-webview";
  description = "Minimal Haskell bindings for the webview library";
  license = lib.licenses.mit;
  __onlyPropagateKnownPkgConfigModules = true;
  mainProgram = "demo";
}
