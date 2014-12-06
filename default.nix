{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: rec {
  pname = "wybor";
  version = "0.1.0";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
  buildDepends = with haskellPackages; [
    ansiTerminal ansiWlPprint conduit lens optparseApplicative resourcet
    semigroups terminalSize text transformers
  ];
  testDepends = with haskellPackages; buildDepends ++ [ hspec ];
  meta = {
    homepage = "https://github.com/supki/wybor";
    description = "Console line fuzzy search";
    license = self.stdenv.lib.licenses.bsd3; # this is a lie!
    platforms = self.ghc.meta.platforms;
  };
})
