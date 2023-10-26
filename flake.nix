{
  description = "Haskell COOL compiler";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    hix = {
      url = "github:tek/hix?ref=0.6.7";
    };
  };

  outputs = {
    nixpkgs,
    hix,
    ...
  }: let
    inherit (nixpkgs) lib;
  in
    hix ({config, ...}: let
      pkgs = nixpkgs.legacyPackages.${config.system};
    in {
      envs.dev = {
        ghc.compiler = "ghc946";
        hls.enable = true;

        buildInputs = with pkgs; [
          mktemp
          alex
        ];
      };

      overrides = {
        hackage,
        unbreak,
        ...
      }: { };

      hackage.versionFile = "ops/version.nix";

      cabal = {
        license = "MIT";
        license-file = "LICENSE";
        author = "reo101";
        component.language = "GHC2021";
        default-extensions = [
          "BlockArguments"
          "DataKinds"
          "DeriveAnyClass"
          "DerivingStrategies"
          "ExplicitForAll"
          "ExplicitNamespaces"
          "LambdaCase"
          "MultiWayIf"
          "OverloadedRecordDot"
          "OverloadedStrings"
          "RecordWildCards"
          "ScopedTypeVariables"
          "TemplateHaskell"
          "TypeFamilies"
          "UndecidableInstances"
          "UnicodeSyntax"
          "ViewPatterns"
        ];
        ghc-options = [
          "-Wall"
          "-Wunused-type-patterns"
          "-Wunused-packages"
          "-Wmissing-deriving-strategies"
          "-Wredundant-constraints"
          "-Widentities"
          "-Wmissing-export-lists"
          "-Wno-name-shadowing"
        ];
      };

      packages.haskool = {
        src = ./.;
        cabal.meta.synopsis = "Haskell COOL compiler";
        override = {nodoc, ...}: nodoc;

        library = {
          enable = true;
          dependencies = [
            "attoparsec"
            "bifunctors"
            "comonad"
            "comonad-extras"
            "containers"
            "directory"
            "extra"
            "filepath"
            "fixed-vector"
            "lens"
            "mtl"
            "regex-tdfa"
            "text"
            "utility-ht"
            "validation"
          ];
        };

        executable.enable = true;

        test = {
          enable = true;
          main = "Spec.hs";
          dependencies = [
            "QuickCheck" # ^>= 2.12"
            "quickcheck-instances"
            "hspec" # ^>= 2.11"
            "hspec-discover" # ^>= 2.11"
            "lens-properties" # ^>= 4.11"
          ];
        };
      };
    });
}
