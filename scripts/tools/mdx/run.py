from docbench import Doclang, template
import subprocess as sp
import shlex
from pathlib import Path
import shutil

JS_TEMPLATE = template(
    """
import React from "react";
import ReactDOM from "react-dom/client";
import Document from "./{{ filename }}";
ReactDOM.createRoot(document.getElementById("root")).render(<Document />);
"""
)

HTML_TEMPLATE = template(
    """
<!DOCTYPE html>
<html>
  <body>
    <div id="root"></div>
    <script>
{{ contents }}
    </script>
  </body>
</html>
"""
)

heredir = Path(__file__).parent


class Mdx(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir, tmpdir):
        node_modules = heredir / "node_modules"
        (tmpdir / "node_modules").symlink_to(node_modules)

        shutil.copy(heredir / "build.mjs", tmpdir / "build.mjs")

        js_path = tmpdir / "input.jsx"
        js_path.write_text(JS_TEMPLATE.render(filename=filename.name))

        cmd = f"node build.mjs"
        output_js = sp.check_output(shlex.split(cmd), cwd=tmpdir).decode("utf-8")

        output_path = output_dir / "mdx.html"
        output_html = HTML_TEMPLATE.render(contents=output_js)
        output_path.write_text(output_html)

        return output_path


Mdx.main()
