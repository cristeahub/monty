#!/usr/bin/env python3
"""Build the released Bonsai stack with its compatible, isolated Dune version."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import urllib.request

root = Path(__file__).resolve().parent
tools = root / '.toolchain'
dune = tools / 'dune'
version = '3.23.0'
checksum = 'e92987ffe4bc7956b18b2251b0f9844d55dfa387035ce60a3f0fd5ac4e2617d4'
if not dune.exists():
    if not shutil.which('ocaml'):
        raise SystemExit('An OCaml compiler is needed once to bootstrap Dune 3.23. Run this from your OCaml development environment.')
    tools.mkdir(exist_ok=True)
    archive = tools / 'dune.tbz'
    with urllib.request.urlopen('https://github.com/ocaml/dune/releases/download/' + version + '/dune-' + version + '.tbz', timeout=60) as response:
        archive.write_bytes(response.read())
    if hashlib.sha256(archive.read_bytes()).hexdigest() != checksum:
        raise SystemExit('Dune archive checksum mismatch')
    subprocess.run(['tar', '-xjf', str(archive), '-C', str(tools)], check=True)
    source = tools / ('dune-' + version)
    subprocess.run(['ocaml', 'boot/bootstrap.ml'], cwd=source, check=True)
    shutil.copy2(source / '_boot/dune.exe', dune)
    archive.unlink()

env = {**os.environ, 'PATH': str(tools) + os.pathsep + os.environ['PATH']}
args = sys.argv[1:] or ['build', '--profile', 'release', 'main.bc.js', 'index.html', 'style.css']
os.chdir(root)
if args[0] in ('build', 'runtest'):
    # js_of_ocaml 5's findlib cannot infer a relocatable compiler's stdlib.
    env['OCAMLLIB'] = subprocess.check_output(
        [str(dune), 'exec', '--', 'ocamlc', '-where'], env=env, text=True).strip()
    config = tools / 'findlib.conf'
    config.write_text('path=' + json.dumps(env['OCAMLLIB']) + '\nstdlib=' + json.dumps(env['OCAMLLIB']) + '\n')
    env['OCAMLFIND_CONF'] = str(config)
os.execve(str(dune), [str(dune), *args], env)
