{ compiler ? "ghc8101" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "utdemir-icfp2020" =
        let orig =
          hself.callCabal2nix
            "utdemir-icfp2020"
            (gitignore ./.) { };
        in
        pkgs.haskell.lib.overrideCabal orig (_: {
          extraLibraries = [ pkgs.mesa ];
        });
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."utdemir-icfp2020"
    ];
    buildInputs = with pkgs.haskellPackages; [
      myHaskellPackages.cabal-install
      ghcid
      ormolu
      hlint
      (import sources.niv { }).niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."utdemir-icfp2020");
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  "utdemir-icfp2020" = myHaskellPackages."utdemir-icfp2020";
}
