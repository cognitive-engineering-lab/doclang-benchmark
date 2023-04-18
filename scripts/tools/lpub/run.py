from docbench import Doclang, template
import subprocess as sp
import shlex
from pathlib import Path
import shutil
import os

heredir = Path(__file__).parent


class LivingPapers(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir, tmpdir):
        node_modules = heredir / "node_modules"
        (tmpdir / "node_modules").symlink_to(node_modules)

        entry_point = filename.name if filename.is_file() else tmpdir / f'{filename.parent.name}.lpub'

        # Note: this generates a bundle.css file we need to inline.        
        cmd = f"./node_modules/.bin/lpub {entry_point}"
        sp.check_call(shlex.split(cmd), cwd=tmpdir)

        html = (tmpdir / "index.html").read_text()
        css_path = tmpdir / "bundle.css"
        js_path = tmpdir / "bundle.js"

        if css_path.exists():
            css = css_path.read_text()
            html = html.replace("""<link rel="stylesheet" href="./bundle.css" />""", f"<style>{css}</style>")

        if js_path.exists():
            js = js_path.read_text()
            html = html.replace("""<script type="module" src="./bundle.js"></script>""", f"<script>{js}</script>")

        (output_dir / "lpub.html").write_text(html)        


LivingPapers.main(debug=True)
