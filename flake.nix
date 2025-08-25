{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc964;
            packages = {
              test-client-sdk.source = ./client-codegen-test/output/source/haskell-client-codegen;
            };
            devShell = {
              hlsCheck.enable = false;
            };
            autoWire = [
              "packages"
              "apps"
              "checks"
            ];
          };
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            nativeBuildInputs =
              let
                jdk = pkgs.jdk17_headless;
              in
              with pkgs;
              [
                # customise the jdk which gradle uses by default
                (callPackage gradle-packages.gradle_8 {
                  java = jdk;
                })
              ];
          };
        };
    };
}
