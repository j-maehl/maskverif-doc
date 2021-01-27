with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "mytool-0";
  src = ./.;
  buildInputs = [ ]
    ++ (with ocamlPackages; [ ocaml findlib dune menhir zarith merlin ocamlgraph])
    ;
  preBuild = "mkdir -p $out/lib/ocaml/${ocaml.version}/site-lib || true";
}
