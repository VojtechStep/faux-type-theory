{
  description = "Faux Type Theory";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ocamlPackages = pkgs.ocamlPackages;
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        name = "faux-type-theory";

        packages = with ocamlPackages; [
          ocaml
          findlib
          dune_3
          sedlex
          menhir
          menhirLib
          bindlib
        ];

        env = {
          MERLIN_MODE = "${ocamlPackages.merlin}/share/emacs/site-lib";
          DUNE_MODE = "${ocamlPackages.dune_3}/share/emacs/site-lib";
        };
      };
    };
}
