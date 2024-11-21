{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-manifest = {
      url = "https://static.rust-lang.org/dist/channel-rust-1.80.0.toml";
      flake = false;
    };
  };

  outputs = inputs @ {
    flake-parts,
    crane,
    fenix,
    rust-manifest,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import inputs.systems;

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = {
        self',
        pkgs,
        lib,
        system,
        ...
      }: let
        cargoToml = lib.importTOML ./Cargo.toml;

        pname = "hayagriva";
        version = cargoToml.package.version;

        # Fenix to manage rust-toolchain configuration
        rust-toolchain = (fenix.packages.${system}.fromManifestFile rust-manifest).defaultToolchain;

        # Crane-based Nix flake configuration.
        # Based on https://github.com/ipetkov/crane/blob/master/examples/trunk-workspace/flake.nix
        craneLib = (crane.mkLib pkgs).overrideToolchain rust-toolchain;

        # Hayagriva files to include in the derivation.
        # Adds it to the nix store
        src = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            ./Cargo.toml
            ./Cargo.lock
            ./rustfmt.toml
            ./archive
            ./docs
            ./styles
            ./tests
            ./src
          ];
        };

        # Hayagriva derivation's args
        commonCraneArgs = {
          inherit src pname version;
          nativeBuildInputs = [pkgs.git];
        };

        # Derivation with just the dependencies avoids rebuilding re-building them.
        cargoArtifacts = craneLib.buildDepsOnly commonCraneArgs;
        hayagriva = craneLib.buildPackage (commonCraneArgs
          // {
            inherit cargoArtifacts;

            # Specify the feature
            cargoExtraArgs = "--features cli --locked";

            # Tests use git to pull dependencies during testPhase breaking sandboxing
            # Otherwise impure evaluation is required
            cargoTestExtraArgs = "--workspace -- --skip always_archive archiver";

            meta.mainProgram = "hayagriva";
          });
      in {
        formatter = pkgs.nixpkgs-fmt;

        packages = {
          default = hayagriva;
          hayagriva-dev = self'.packages.default;
        };

        overlayAttrs = builtins.removeAttrs self'.packages ["default"];

        apps.default = {
          type = "app";
          program = lib.getExe hayagriva;
        };

        checks = {
          typst-fmt = craneLib.cargoFmt commonCraneArgs;
          typst-clippy = craneLib.cargoClippy (commonCraneArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--workspace -- --deny warnings";
            });
          typst-test = craneLib.cargoTest (commonCraneArgs
            // {
              inherit cargoArtifacts;
              # See note a tests during run time
              cargoTestExtraArgs = "--workspace -- --skip always_archive archiver";
            });
        };

        devShells.default = craneLib.devShell {
          checks = self'.checks;
          inputsFrom = [hayagriva];

          packages = [
            # A script for quickly running tests.
            # See https://github.com/typst/typst/blob/main/tests/README.md#making-an-alias
            (pkgs.writeShellScriptBin "testit" ''
              cargo test -- "$@"
            '')
          ];
        };
      };
    };
}
