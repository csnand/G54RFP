(TeX-add-style-hook
 "Report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=0.75in") ("biblatex" "style=numeric")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "cite"
    "inputenc"
    "setspace"
    "geometry"
    "biblatex"
    "float"
    "graphicx")
   (LaTeX-add-labels
    "fig:taskI.1"
    "fig:sheet")
   (LaTeX-add-bibliographies
    "../bibs/ref"))
 :latex)

