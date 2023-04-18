from docbench import Doclang, template
import subprocess as sp
import shlex
from pathlib import Path

TEMPLATE = template(
    r"""
#lang scribble/base

@(require scribble/core scribble/html-properties)

{{ contents }}
"""
)


class Latex(Doclang):
    def install(self):
        try:
            sp.check_call(shlex.split("which scribble"))
        except sp.CalledProcessError:
            sp.check_call(shlex.split("raco pkg install scribble"))

    def run(self, filename, output_dir, tmpdir):
        contents = filename.read_text()
        elaborated = TEMPLATE.render(contents=contents)
        elaborated_filename = Path(tmpdir) / filename.name
        elaborated_filename.write_text(elaborated)

        sp.check_call(shlex.split(f"scribble {elaborated_filename}"), cwd=tmpdir)
        output_filename = Path(tmpdir) / f"{filename.stem}.html"
        output_filename.rename(output_dir / "scrbl.html")

        return output_filename


Latex.main()
