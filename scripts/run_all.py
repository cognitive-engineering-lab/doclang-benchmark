import os
from pathlib import Path
import subprocess as sp
import shlex

REPO_ROOT = Path(__file__).parent.parent
TASK_DIR = REPO_ROOT / "tasks"
TOOL_DIR = REPO_ROOT / "scripts" / "tools"

TASKS = os.listdir(TASK_DIR)
TOOLS = os.listdir(TOOL_DIR)


for task in TASKS:
    for tool in TOOLS:
        try:
            file_path = TASK_DIR / task / f"{task}.{tool}"
            cmd = f"python3 run.py run {file_path}"
            cwd = TOOL_DIR / tool
            sp.check_call(shlex.split(cmd), cwd=cwd)
        except sp.CalledProcessError:
            print(f"Failure: task: {task} / tool: {tool}")
