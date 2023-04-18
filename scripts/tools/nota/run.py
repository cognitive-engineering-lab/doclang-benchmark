from docbench import Doclang
import subprocess as sp
import shlex
from pathlib import Path

heredir = Path(__file__).parent


class LivingPapers(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir, tmpdir):
        node_modules = heredir / "node_modules"
        (tmpdir / "node_modules").symlink_to(node_modules)

        cmd = f"./node_modules/.bin/nota build {filename.name}"
        sp.check_call(shlex.split(cmd), cwd=tmpdir)

        output_subdir = output_dir / "nota"
        output_subdir.mkdir(exist_ok=True)

        dist = tmpdir / "dist"
        files = [dist / "index.html", dist / "index.css", dist / "index.mjs"]
        for f in files:
            if f.exists():
                f.rename(output_subdir / f.name)

        return output_subdir / "index.html"


LivingPapers.main(debug=True)
