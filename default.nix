with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "mask-verif";
  src = ./.;
  buildInputs = [ ]
    ++ (with ocamlPackages; [ ocaml findlib menhir zarith merlin ocamlgraph])
    ;
}
