import os
from pathlib import Path
import subprocess as sp
import shlex
import argparse
import toml

REPO_ROOT = Path(__file__).parent.parent
TASK_DIR = REPO_ROOT / "tasks"
LANG_DIR = REPO_ROOT / "scripts" / "langs"
OUTPUT_DIR = REPO_ROOT / "output"

TASKS = os.listdir(TASK_DIR)
LANGS = os.listdir(LANG_DIR)


def run_one(task, lang):
    try:
        impl_path = TASK_DIR / task / f"{task}.{lang}"
        if not impl_path.exists():
            impl_dir = TASK_DIR / task / lang
            if not impl_dir.exists():
                print("Warning: missing file for lang {lang} on task {task}")
                return None
            impl_path = impl_dir / f"{task}.{lang}"

        print(f"Running task {task} with lang {lang}")
        cmd = f"python3 run.py run {impl_path}"
        cwd = LANG_DIR / lang
        stdout = sp.check_output(shlex.split(cmd), cwd=cwd).decode("utf-8")
        return Path(stdout.splitlines()[-1])

    except sp.CalledProcessError:
        print(f"Failure: task: {task} / lang: {lang}")
        return None


def run_install(lang):
    cmd = "python3 run.py install"
    cwd = LANG_DIR / lang
    sp.check_call(shlex.split(cmd), cwd=cwd)


def load_exclude(task):
    exclude_path = TASK_DIR / task / "exclude.toml"
    return (
        toml.loads(exclude_path.read_text())["exclude"] if exclude_path.exists() else []
    )


def cli():
    parser = argparse.ArgumentParser()
    subcommands = parser.add_subparsers(dest="subcommand")
    one_cmd = subcommands.add_parser("one")
    one_cmd.add_argument("task")
    one_cmd.add_argument("lang")
    one_cmd.add_argument("--open", action="store_true")
    subcommands.add_parser("all")
    subcommands.add_parser("install")
    return parser.parse_args()


args = cli()

if args.subcommand == "one":
    output_path = run_one(args.task, args.lang)
    if args.open:
        sp.check_call(["open", output_path])

elif args.subcommand == "all":
    for task in TASKS:
        exclude = load_exclude(task)
        for lang in LANGS:
            if lang in exclude:
                continue
            run_one(task, lang)

elif args.subcommand == "install":
    for lang in LANGS:
        run_install(lang)
