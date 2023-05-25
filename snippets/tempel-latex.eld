;;; Code snippets for LaTeX.

;; Figure.
(fig
"\\begin{figure}[" (p "h") "]"
n> "\\centering"
n> "\\includegraphic[width=" (p "0.8") "\\linewidth]{" p "}"
n> "\\caption{" p "}"
n> "\\label{fig:" p "}"
n "\\end{figure}"
n q
)

;; 1x2 figures.
(fig2
"\\begin{figure}[" (p "h") "]"
n> "\\centering"
n> "\\begin{minipage}{" (p "0.45") "\\linewidth}"
n> > "\\centering"
n> > "\\includegraphic[width=" (p "0.8") "\\linewidth]{" p "}"
n> > "\\caption{" p "}"
n> > "\\label{fig:" p "}"
n> "\\end{minipage}"
n> "\\hfill"
n> "\\begin{minipage}{" (p "0.45") "\\linewidth}"
n> > "\\centering"
n> > "\\includegraphic[width=" (p "0.8") "\\linewidth]{" p "}"
n> > "\\caption{" p "}"
n> > "\\label{fig:" p "}"
n> "\\end{minipage}"
n "\\end{figure}"
n q
)

;; 1x2 sub-figures.
(figs2
"\\begin{figure}[" (p "h") "]"
n> "\\centering"
n> "\\begin{minipage}{" (p "0.45") "\\linewidth}"
n> > "\\centering"
n> > "\\includegraphic[width=" (p "0.8") "\\linewidth]{" p "}"
n> > "\\subcaption{" p "}"
n> > "\\label{fig:" p "}"
n> "\\end{minipage}"
n> "\\hfill"
n> "\\begin{minipage}{" (p "0.45") "\\linewidth}"
n> > "\\centering"
n> > "\\includegraphic[width=" (p "0.8") "\\linewidth]{" p "}"
n> > "\\subcaption{" p "}"
n> > "\\label{fig:" p "}"
n> "\\end{minipage}"
n> "\\caption{" p "}"
n> "\\label{fig:" p "}"
n "\\end{figure}"
n q
)

;; Table.
(tab
"\\begin{table}[" (p "h") "]"
n> "\\centering"
n> "\\caption{" p "}"
n> "\\label{tab:" p "}"
n> "\\begin{tabular}{" p "}"
n> > "\\hline"
n> > p " \\\\"
n> > "\\hline"
n> > q
n> > "\\hline"
n> "\\end{tabular}"
n "\\end{table}"
n
)

;; Table with booktabs.
(tabb
"\\begin{table}[" (p "h") "]"
n> "\\centering"
n> "\\caption{" p "}"
n> "\\label{tab:" p "}"
n> "\\begin{tabular}{" p "}"
n> > "\\toprule"
n> > p " \\\\"
n> > "\\midrule"
n> > q
n> > "\\bottomrule"
n> "\\end{tabular}"
n "\\end{table}"
n
)

;; Equation.
(equ
"\\begin{equation}"
n> "\\label{equ:" p "}"
n> q
n "\\end{equation}"
n
)
