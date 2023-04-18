import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from jinja2 import Environment
import tempfile
from contextlib import contextmanager

REPO_ROOT = Path(__file__).parent.parent.parent
JINJA_ENV = Environment()


def template(s):
    return JINJA_ENV.from_string(s)


@contextmanager
def persistent_tmpdir():
    yield tempfile.mkdtemp()


class Doclang(ABC):
    @abstractmethod
    def install(self):
        pass

    @abstractmethod
    def run(self, filename: Path, output_dir: Path, tmpdir: Path):
        pass

    @classmethod
    def main(cls, debug=False):
        parser = argparse.ArgumentParser()
        subparsers = parser.add_subparsers(dest="subcommand", required=True)

        subparsers.add_parser("install")

        run_parser = subparsers.add_parser("run")
        run_parser.add_argument("filename")

        args = parser.parse_args()

        doclang = cls()
        if args.subcommand == "install":
            doclang.install()
        elif args.subcommand == "run":
            input = Path(args.filename).resolve()

            output_dir = REPO_ROOT / "output" / input.parent.name
            output_dir.mkdir(parents=True, exist_ok=True)

            tmpdir_ctx = persistent_tmpdir() if debug else tempfile.TemporaryDirectory()

            with tmpdir_ctx as tmpdir:
                if debug:
                    print(tmpdir)
                doclang.run(input, output_dir, tmpdir=Path(tmpdir))
