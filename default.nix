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
    url = "https://github.com/chiroptical/unison-abilities/archive/v0.1a0.tar.gz";
    sha256 = "0qcp7zhhssip3an4bzfj6s9sk62zxzhzq45m2v2y58cwnx2yix12";
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
