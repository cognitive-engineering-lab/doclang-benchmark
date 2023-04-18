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

            output_dir = REPO_ROOT / "output" / input.parent.name
            output_dir.mkdir(parents=True, exist_ok=True)

            tmpdir_ctx = persistent_tmpdir() if debug else tempfile.TemporaryDirectory()

            with tmpdir_ctx as tmpdir:
                tmpdir = Path(tmpdir)

                if input.is_file():
                    shutil.copy(input, tmpdir / input.name)
                else:
                    for file in os.listdir(input):
                        shutil.copy(input / file, tmpdir / file)

                if debug:
                    print(tmpdir)

                doclang.run(input, output_dir, tmpdir=tmpdir)
