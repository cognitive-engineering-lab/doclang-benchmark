from docbench import Doclang
import subprocess as sp
import shlex


class Markdown(Doclang):
    def install(self):
        sp.check_call(shlex.split("npm install"))

    def run(self, filename, output_dir):
        contents = sp.check_output(
            shlex.split(f"npm exec commonmark {filename}")
        ).decode("utf-8")
        output = output_dir / "markdown.html"
        output.write_text(contents)


Markdown.main()
