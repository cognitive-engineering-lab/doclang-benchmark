from docbench import Doclang, template
import subprocess as sp
import shlex
from pathlib import Path

JS_TEMPLATE = template(
    """
import React, {useState} from "react";
import ReactDOM from "react-dom/client";
let Component = (() => {{ contents }});
ReactDOM.createRoot(document.getElementById("root")).render(<Component />);
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


class Jsx(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir, tmpdir):
        node_modules = Path(__file__).parent / "node_modules"
        (tmpdir / "node_modules").symlink_to(node_modules)

        elaborated = JS_TEMPLATE.render(contents=filename.read_text())
        elaborated_path = tmpdir / "input.jsx"
        elaborated_path.write_text(elaborated)

        cmd = f"./node_modules/.bin/esbuild --bundle {elaborated_path}"
        output_js = sp.check_output(shlex.split(cmd)).decode("utf-8")

        output_path = output_dir / "jsx.html"
        output_html = HTML_TEMPLATE.render(contents=output_js)
        output_path.write_text(output_html)

        return output_path


Jsx.main()
