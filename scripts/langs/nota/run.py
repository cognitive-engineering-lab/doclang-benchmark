from docbench import Doclang, template
import subprocess as sp
import shlex
from pathlib import Path

heredir = Path(__file__).parent

NOTA_TEMPLATE = template(
    """
% import {useState} from "react";
{{ contents }}
"""
)


class Nota(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir, tmpdir):
        node_modules = heredir / "node_modules"
        (tmpdir / "node_modules").symlink_to(node_modules)

        contents = filename.read_text()
        elaborated = NOTA_TEMPLATE.render(contents=contents)
        elaborated_path = tmpdir / "index.nota"
        elaborated_path.write_text(elaborated)

        cmd = f"./node_modules/.bin/nota build {elaborated_path}"
        sp.check_call(shlex.split(cmd), cwd=tmpdir)

        output_subdir = output_dir / "nota"
        output_subdir.mkdir(exist_ok=True)

        dist = tmpdir / "dist"
        files = [dist / "index.html", dist / "index.css", dist / "index.mjs"]
        for f in files:
            if f.exists():
                f.rename(output_subdir / f.name)

        return output_subdir / "index.html"


Nota.main(debug=True)
