{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [ 
    ghc
    haskellPackages.haskell-language-server
    cabal-install
    ormolu
  ];
}
