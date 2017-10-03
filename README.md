# annot-render -- Coding assignment feedback renderer

Annot-render is a tool for generating nice-looking and intuitive feedback for
coding assignments. It gathers free-form general feedback and annotated source
files using the annotation export format of the annotate.el Emacs package and
generates LaTeX which can be compiled to a nice-looking PDF>.

The current implementation specifically targets the course structure at the
Department of Computer Science at the University of Copenhagen and a number of
restrictions present in the tool originates from that, However, these
restrictions can be lifted through trivial modifications to the program.

## Acknowledgments
 * The diff parsing module is based on the diff-parse package by Gabe Mulley
 * LaTeX/TikZ magic taken from https://tex.stackexchange.com/a/87179
