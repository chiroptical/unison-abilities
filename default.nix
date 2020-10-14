{ packages ? import ./pkgs.nix }:
let
  inherit (packages) pkgs;

  inputs = [
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-basic
        beamer
        etoolbox
        translator
        minted
        fvextra
        fancyvrb
        upquote
        lineno
        catchfile
        xstring
        framed
        float
        helvetic
        cancel;
    })
    pkgs.python38Packages.pygments
    pkgs.which
  ];
in
pkgs.stdenv.mkDerivation {
  name = "unison-abilities";
  buildInputs = inputs;
  src = builtins.fetchurl {
    url = "https://github.com/chiroptical/unison-abilities/archive/v0.1.tar.gz";
    sha256 = "1fnd4dg7yyhy3jy30jjidy03xa1byb3g89nqg57qksqls7jkpz0k";
  };
  buildPhase = ''
    source $stdenv/setup
    mkdir -p $out
    pdflatex -shell-escape ./unison-abilities.tex
    pdflatex -shell-escape ./unison-abilities.tex
    pdflatex -shell-escape ./unison-abilities.tex
  '';
  dontInstall = true;
}
