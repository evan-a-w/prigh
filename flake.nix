{
  description = "prigh — OCaml backend and Bonsai_term frontend on OxCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix.url = "github:tweag/opam-nix";

    # The patched packages/compiler (Jane Street `+ox` versions) live in the
    # OxCaml opam repository. It is merged *before* upstream opam-repository so
    # its definitions take precedence.
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    ox-opam-repository = {
      url = "github:oxcaml/opam-repository";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      opam-nix,
      opam-repository,
      ox-opam-repository,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        repos = [
          ox-opam-repository
          opam-repository
        ];

        # Pinned to the exact versions of the working `prigh-ox` switch. This
        # both forces the OxCaml compiler and keeps opam-nix's solver inside its
        # 60 s IFD timeout. See nix/prigh-ox-pins.nix.
        query = import ./nix/prigh-ox-pins.nix;

        # OxCaml 5.2.0+ox native codegen drops the second addressing register
        # of `Ifloatarithmem` (any float-array arithmetic at the non-AVX
        # baseline); the patch makes it copy the whole arg array like upstream
        # OCaml. See nix/fix-floatarithmem.patch.
        overlay =
          final: prev:
          {
            oxcaml-compiler = prev.oxcaml-compiler.overrideAttrs (oa: {
              patches = (oa.patches or [ ]) ++ [ ./nix/fix-floatarithmem.patch ];
            });
          };

        # `depopts = false` mirrors a plain `opam install` and avoids pulling in
        # test-only optional deps (e.g. markup -> bisect_ppx -> cmdliner < 2).
        #
        # NOTE: this scope is the *Bonsai_term frontend* toolchain. The vanilla
        # `backend/` does not currently build under OxCaml (digestif does not
        # compile with modes, and the installed mirage-crypto-rng is 0.11.x
        # while the backend uses the 2.x `use_default` API), so the backend
        # stays on the vanilla `prigh` switch for now.
        scope = (on.buildOpamProject' {
          inherit repos;
          resolveArgs = {
            depopts = false;
            dev = false;
          };
        } ./spike/bonsai_term_hello query).overrideScope overlay;
      in
      {
        legacyPackages = scope;

        packages.default = scope.bonsai_term_hello;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ scope.bonsai_term_hello ];
          buildInputs = [
            scope.bonsai_test
            scope.notty_async
            scope.expect_test_helpers_core
            pkgs.nodejs
            pkgs.ripgrep
          ];
        };
      }
    );
}
