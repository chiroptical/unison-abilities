build:
	nix-shell --command 'pdflatex -shell-escape unison-abilities.tex; pdflatex -shell-escape unison-abilities.tex; pdflatex -shell-escape unison-abilities.tex'
