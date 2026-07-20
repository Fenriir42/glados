{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    git-hooks,
  }: let
    inherit (nixpkgs) lib;

    genSystems = lib.genAttrs ["x86_64-linux"];

    eachSystem = f:
      genSystems
      (system: f nixpkgs.legacyPackages.${system});

    eachSystem' = f:
      genSystems
      (system: f self.shared.${system});
  in {
    checks = eachSystem' (
      {
        pkgs,
        haskell,
        ...
      }: {
        pre-commit-check = git-hooks.lib.${pkgs.system}.run {
          src = ./.;
          hooks =
            {
              commit-name = {
                enable = true;
                name = "commit name";
                stages = ["commit-msg"];
                entry = ''
                  ${pkgs.python310.interpreter} ${./scripts/apply-commit-convention.py}
                '';
              };
            }
            // (lib.genAttrs [
                "alejandra"
                "cabal-fmt"
                "ormolu"
                "hlint"
              ] (x: {
                enable = true;
                package = lib.getBin (haskell.${x} or pkgs.${x});
              }));
        };
      }
    );

    formatter = eachSystem (pkgs: pkgs.alejandra);

    devShells = eachSystem' ({
      pkgs,
      haskell,
      ...
    }: {
      default = pkgs.mkShell {
        inherit (self.checks.${pkgs.system}.pre-commit-check) shellHook;

        buildInputs = with pkgs; [
          zlib
          zstd
        ];

        packages = with pkgs;
          [
            alejandra
            chez
            curl
            jq
            pkg-config
          ]
          ++ self.checks.${pkgs.system}.pre-commit-check.enabledPackages
          ++ (with haskell; [
            cabal-install
            haskell-language-server

            (ghcWithPackages (p: [
              Cabal
              Cabal-syntax
              containers
              aeson
              bytestring
              containers
              hpc-codecov
              hspec
              hspec-expectations
              megaparsec
              network
              optparse-applicative
              parsec
              parsec
              pretty-simple
              regex-tdfa
              silently
            ]))
          ])
          ++ (with pkgs; [
            pnpm
            nodejs
          ]);
      };
    });

    packages = eachSystem' ({
      pkgs,
      hpkgs,
      ...
    }: let
      stdlib = pkgs.runCommand "glados-stdlib" {} ''
        mkdir -p $out
        cp ${./std}/*.qa $out/
      '';
    in {
      default = pkgs.symlinkJoin {
        name = "glados";
        paths = [hpkgs.cli];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/cli \
            --set QUANT_STDLIB ${stdlib}
        '';
      };

      lsp = pkgs.symlinkJoin {
        name = "glados-lsp";
        paths = [hpkgs.lsp-server];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/glados-lsp \
            --set QUANT_STDLIB ${stdlib}
        '';
      };

      repl = pkgs.symlinkJoin {
        name = "glados-repl";
        paths = [hpkgs.repl];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/glados-repl \
            --set QUANT_STDLIB ${stdlib}
        '';
      };
    });

    shared = eachSystem (pkgs: let
      ghc = pkgs.haskell.packages.ghc984;
    in {
      inherit pkgs;

      haskell = ghc;

      hpkgs = ghc.override {
        overrides = final: _prev: {
          ast = final.callCabal2nix "ast" ./ast {};
          cabal-extract = final.callCabal2nix "cabal-extract" ./cabal-extract {};
          parser = final.callCabal2nix "parser" ./parser {};
          typechecker = final.callCabal2nix "typechecker" ./typechecker {};
          compiler = final.callCabal2nix "compiler" ./compiler {};
          vm = final.callCabal2nix "vm" ./vm {};
          formatter = final.callCabal2nix "formatter" ./formatter {};
          lsp-server = final.callCabal2nix "lsp-server" ./lsp-server {};
          repl = final.callCabal2nix "repl" ./repl {};
          cli = final.callCabal2nix "cli" ./cli {};
        };
      };
    });
  };
}
