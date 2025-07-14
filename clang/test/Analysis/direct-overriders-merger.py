#!/usr/bin/env python
#
# ===- direct-overriders-merger.py -----------------------------*- python -*-===#
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
# ===------------------------------------------------------------------------===#

r"""
Usage:
  direct-overriders-merger.py <directory-to-walk> <output-file>

This script collects all the `.direct-overriders` files in the specified directory
and merges them into a single output file. Each line in the output file contains
the number of direct overriders followed by the user (USR) identifier, and then
the list of direct overriders.

Example:
  // RUN: %direct-overriders-merger.py %t %t.direct-overriders
"""

import os
import re
import sys

directory_path = sys.argv[1]
output_file = sys.argv[2]

num_and_usr_pattern = re.compile(r'^(\d+) (.*)$')

def parse_file(file_path):
    result = {}
    with open(file_path, 'r') as f:
        lines = f.readlines()
        i = 0
        while i < len(lines):
            line = lines[i][:-1]
            i += 1
            m = num_and_usr_pattern.match(line)
            if m:
                direct_overrider_usrs = set()
                num = int(m.group(1))
                usr = m.group(2)
                for j in range(0, num):
                    direct_overrider_usrs.add(lines[i][:-1])
                    i += 1
                if usr not in result:
                    result[usr] = set()
                result[usr].update(direct_overrider_usrs)
    return result

result = {}
for root, _, files in os.walk(directory_path):
    for file in files:
        if file.endswith('.direct-overriders'):
            file_path = os.path.join(root, file)
            try:
                result = {**result, **parse_file(file_path)}
            except Exception as e:
                print(f"Error reading file {file_path}: {e}")


# Write the collected data to the output file
try:
    with open(output_file, 'w') as out_file:
        for usr, overriders in result.items():
            out_file.write(f"{len(overriders)} {usr}\n")
            for overrider in overriders:
                out_file.write(f"{overrider}\n")
    print(f"Collected data written to {output_file}")
except Exception as e:
    print(f"Error writing to output file {output_file}: {e}")

