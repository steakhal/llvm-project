#!/usr/bin/python3
"""SSAF link wrapper.

Wraps a real link/archive command, then runs clang-ssaf-linker to produce
a linked .json summary alongside the output binary or archive.

Usage (as CMAKE_LIBTOOL for static libs, via ssaf-libtool.sh):
    ssaf-wrapper.py libtool -static -o lib/libFoo.a obj1.o obj2.o ...
    -> Produces lib/libFoo.a.json (manifest listing .o.json paths)

Usage (as prefix in CMAKE_CXX_LINK_EXECUTABLE):
    ssaf-wrapper.py clang++ <flags> obj1.o obj2.o -o bin/exe lib/libBar.a ...
    -> Reads .a.json manifests, collects all .o.json paths
    -> Runs clang-ssaf-linker to produce bin/exe.json (linked LU summary)
"""

import json
import os
import subprocess
import sys

SSAF_LINKER = '/Users/benics/git/upstream-llvm-ssaf/build/reldeb/bin/clang-ssaf-linker'


def parse_target_and_inputs(args):
    """Parse -o <target> and collect .o/.a input files from the command args."""
    target = None
    inputs = []

    i = 0
    while i < len(args):
        arg = args[i]
        if arg == '-o' and i + 1 < len(args):
            target = args[i + 1]
            i += 2
            continue
        if arg.endswith('.o') or arg.endswith('.a'):
            inputs.append(arg)
        i += 1

    return target, inputs


def is_archive_command(args):
    """Check if this is a static library creation (libtool/ar) command."""
    # libtool -static or ar commands
    return any(arg == '-static' for arg in args) or \
           any('libtool' in arg or '/ar' in arg for arg in args[:2])


def create_manifest(target, inputs):
    """Create a manifest .json file listing the TU summary paths for a .a."""
    output = target + '.json'
    summaries = []
    for f in inputs:
        if f.endswith('.o'):
            json_path = f + '.json'
            if os.path.exists(json_path):
                summaries.append(os.path.abspath(json_path))

    if not summaries:
        return

    manifest = {'ssaf_manifest': True, 'summaries': summaries}
    with open(output, 'w') as fh:
        json.dump(manifest, fh)
    print(f"SSAF manifest: {len(summaries)} summaries -> {output}")


def link_executable(target, inputs):
    """Collect all TU summaries and run clang-ssaf-linker for an executable."""
    output = target + '.json'
    summaries = []

    for f in inputs:
        json_path = f + '.json'
        if not os.path.exists(json_path):
            continue

        if f.endswith('.a'):
            # Read manifest to get individual TU summary paths
            with open(json_path) as fh:
                manifest = json.load(fh)
            if isinstance(manifest, dict) and manifest.get('ssaf_manifest'):
                summaries.extend(manifest['summaries'])
            # else: not an SSAF manifest, skip
        elif f.endswith('.o'):
            summaries.append(os.path.abspath(json_path))

    if not summaries:
        return

    # Remove stale output
    if os.path.exists(output):
        os.remove(output)

    cmd = [SSAF_LINKER] + summaries + ['-o', output]
    print(f"SSAF linking {len(summaries)} summaries -> {output}")
    result = subprocess.run(cmd)

    if result.returncode != 0:
        print(f"SSAF linker failed for {output}", file=sys.stderr)


def main():
    if len(sys.argv) < 2:
        print("ssaf-wrapper: no command given", file=sys.stderr)
        sys.exit(1)

    real_cmd = sys.argv[1:]

    # 1. Run the real command
    result = subprocess.run(real_cmd)
    if result.returncode != 0:
        sys.exit(result.returncode)

    # 2. Parse target and inputs
    target, inputs = parse_target_and_inputs(real_cmd)
    if not target or not inputs:
        return

    # 3. Archive vs executable
    if is_archive_command(real_cmd):
        create_manifest(target, inputs)
    else:
        link_executable(target, inputs)


if __name__ == '__main__':
    main()
