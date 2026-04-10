#!/bin/bash
# SSAF libtool wrapper: runs real libtool, then ssaf-linker
exec python3 /Users/benics/git/upstream-llvm-ssaf/ssaf-wrapper.py \
  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/libtool \
  "$@"
