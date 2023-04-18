from docbench import Doclang, template
import subprocess as sp
import shlex

TEMPLATE = template(
    r"""
<!DOCTYPE html>
<html>
    <body>
        {{ contents }}
    </body>
</html>
"""
)


class Markdown(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir, **kwargs):
        contents = sp.check_output(
            shlex.split(f"npm exec commonmark {filename}")
        ).decode("utf-8")
        output = output_dir / "md.html"
        output.write_text(TEMPLATE.render(contents=contents))


Markdown.main()
