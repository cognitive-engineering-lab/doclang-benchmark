import os
from pathlib import Path
import subprocess as sp
import shlex
import argparse
import toml

REPO_ROOT = Path(__file__).parent.parent
TASK_DIR = REPO_ROOT / "tasks"
TOOL_DIR = REPO_ROOT / "scripts" / "tools"
OUTPUT_DIR = REPO_ROOT / "output"

TASKS = os.listdir(TASK_DIR)
TOOLS = os.listdir(TOOL_DIR)


def run_one(task, tool):
    try:
        impl_path = TASK_DIR / task / f"{task}.{tool}"
        if not impl_path.exists():
            impl_path = TASK_DIR / task / tool
            if not impl_path.exists():
                print("Warning: missing file for tool {tool} on task {task}")
                return None                

        cmd = f"python3 run.py run {impl_path}"
        cwd = TOOL_DIR / tool
        sp.check_call(shlex.split(cmd), cwd=cwd)

        # TODO: generalize to all extensions
        return OUTPUT_DIR / task / f"{tool}.html"
    except sp.CalledProcessError:
        print(f"Failure: task: {task} / tool: {tool}")
        return None


def run_install(tool):
    cmd = "python3 run.py install"
    cwd = TOOL_DIR / tool
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
    one_cmd.add_argument("tool")
    one_cmd.add_argument("--open", action="store_true")
    subcommands.add_parser("all")
    subcommands.add_parser("install")
    return parser.parse_args()


args = cli()

if args.subcommand == "one":
    output_path = run_one(args.task, args.tool)
    if args.open:
        sp.check_call(["open", output_path])

elif args.subcommand == "all":
    for task in TASKS:
        exclude = load_exclude(task)
        for tool in TOOLS:
            if tool in exclude:
                continue
            run_one(task, tool)

elif args.subcommand == "install":
    for tool in TOOLS:
        run_install(tool)
