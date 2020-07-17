let
  sources = import ./nix/sources.nix;
  pkgsOrig =
    let
      basePkgs = import sources.nixpkgs {};
      patched = basePkgs.applyPatches {
        name = "nixpkgs-patched";
        src = sources.nixpkgs;
        patches = [
          ./patches/0001-Revert-ghc-8.6.3-binary-8.6.5-binary.patch
        ];
      };
    in
      import patched { config.allowBroken = true; };

  pkgsMusl = pkgsOrig.pkgsMusl;

  gitignore = pkgsOrig.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  staticLibs = with pkgsMusl; [
    zlib.static
    (libffi.override { stdenv = makeStaticLibraries stdenv; })
    (gmp.override { withStatic = true; })
  ];

  myHaskellPackages = pkgsMusl.haskell.packages.ghc8101.override {
    overrides = hself: hsuper: {
      "utdemir-icfp2020" =
       let orig = hself.callCabal2nix
            "utdemir-icfp2020"
            (gitignore ./.)
            {};
      in
        pkgsMusl.haskell.lib.overrideCabal orig (_: {
          extraLibraries = staticLibs;
        });
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."utdemir-icfp2020"
    ];
    buildInputs = [
      myHaskellPackages.cabal-install
      pkgsOrig.haskellPackages.ghcid
      pkgsOrig.haskellPackages.ormolu
      pkgsOrig.haskellPackages.steeloverseer
      pkgsOrig.nixpkgs-fmt
    ];
    withHoogle = false;
  };

  exe = pkgsMusl.haskell.lib.justStaticExecutables (myHaskellPackages."utdemir-icfp2020");
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  "utdemir-icfp2020" = myHaskellPackages."utdemir-icfp2020";
}
