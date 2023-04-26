import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from jinja2 import Environment
import tempfile
from contextlib import contextmanager
import shutil
import os

REPO_ROOT = Path(__file__).parent.parent.parent
JINJA_ENV = Environment()


def template(s):
    return JINJA_ENV.from_string(s)


@contextmanager
def persistent_tmpdir():
    yield tempfile.mkdtemp()


def cli():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="subcommand", required=True)

    subparsers.add_parser("install")

    run_parser = subparsers.add_parser("run")
    run_parser.add_argument("filename")

    return parser.parse_args()


class Doclang(ABC):
    @abstractmethod
    def install(self):
        pass

    @abstractmethod
    def run(self, filename: Path, output_dir: Path, tmpdir: Path):
        pass

    @classmethod
    def main(cls, debug=False):
        doclang = cls()
        args = cli()

        if args.subcommand == "install":
            doclang.install()
        elif args.subcommand == "run":
            input = Path(args.filename).resolve()
            rel_input = input.relative_to(REPO_ROOT)

            task = rel_input.parts[1]
            output_dir = REPO_ROOT / "output" / task
            output_dir.mkdir(parents=True, exist_ok=True)

            tmpdir_ctx = persistent_tmpdir() if debug else tempfile.TemporaryDirectory()

            with tmpdir_ctx as tmpdir:
                tmpdir = Path(tmpdir)

                # if input has multiple files it should have the path
                # tasks/<task>/<lang>/<main file>                
                if len(rel_input.parts) == 4:                    
                    for file in os.listdir(input.parent):                        
                        shutil.copy(input.parent / file, tmpdir / file)
                else:
                   shutil.copy(input, tmpdir / input.name)

                if debug:
                    print(tmpdir)

                output_path = doclang.run(input, output_dir, tmpdir=tmpdir)
                print(output_path)
