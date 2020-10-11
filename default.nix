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
        helvetic;
    })
    pkgs.python38Packages.pygments
    pkgs.which
  ];
in
pkgs.stdenv.mkDerivation {
  name = "unison-abilities";
  buildInputs = inputs;
  src = builtins.fetchurl {
    url = "https://github.com/chiroptical/unison-abilities/archive/v0.0.1.tar.gz";
    sha256 = "09hhrv2ijgnbz53zz107wv80w31lyc76skmi92ry8bg6d701g496";
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
