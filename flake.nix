{
  description = "Haskell COOL compiler";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    hix = {
      url = "github:tek/hix?ref=0.6.9";
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
        ghc = {
          compiler = "ghc94";
        };
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
          "DuplicateRecordFields"
          "ExplicitForAll"
          "ExplicitNamespaces"
          "GADTSyntax"
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
            "fixed-vector" "lens"
            "megaparsec"
            "mtl"
            "nonempty-containers"
            "parser-combinators"
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
            "HUnit"
            "hspec" # ^>= 2.05"
            "hspec-discover" # ^>= 2.05"
            "lens-properties" # ^>= 4.05"
            "text"
          ];
        };
      };
    });
}
