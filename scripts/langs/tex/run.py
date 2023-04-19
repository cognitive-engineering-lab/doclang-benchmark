from docbench import Doclang, template
import subprocess as sp
import shlex
from pathlib import Path

TEMPLATE = template(
    r"""
\documentclass[11pt]{article}
\usepackage{xcolor}

\begin{document}
{{ contents }}
\end{document}
"""
)


class Latex(Doclang):
    def install(self):
        try:
            sp.check_call(shlex.split("which pdflatex"))
        except sp.CalledProcessError:
            raise Exception("You have to have pdflatex installed")

    def run(self, filename, output_dir, tmpdir):
        contents = filename.read_text()
        elaborated = TEMPLATE.render(contents=contents)
        elaborated_filename = Path(tmpdir) / filename.name
        elaborated_filename.write_text(elaborated)

        sp.check_call(shlex.split(f"pdflatex {elaborated_filename}"), cwd=tmpdir)
        output_filename = Path(tmpdir) / f"{filename.stem}.pdf"
        output_filename.rename(output_dir / "tex.pdf")

        return output_filename


Latex.main()
