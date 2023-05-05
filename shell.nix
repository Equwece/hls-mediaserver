{ compiler ? "ghc902" }:
let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };


  myNixPkgs = import (src) {
    overlays = [myNixPkgsOverlay];
  };


  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskell.packages."${compiler}".override (oldHaskellPkgs: {
      overrides = haskellPackagesNew: oldHaskellPkgs: rec {
        myProject = haskellPackagesNew.callCabal2nix "hls-mediaserver" ./. {};
      };
    });
  });

  myDevTools = with myNixPkgs; [
    cabal-install 
    haskell-language-server
    cabal2nix
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.myProject.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
