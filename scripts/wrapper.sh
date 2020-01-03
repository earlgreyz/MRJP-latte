#!/bin/bash

# Latte file to compile.
LAT=${1}
# Base name for the compiled files.
BASE="${LAT%.lat}"
# Readable llvm format.
LL="${BASE}.ll"
# Llvm bitcode.
BC="${BASE}.bc"
# Runtime library.
RUNTIME="./lib/runtime.bc"
# Obj file.
OBJ="${BASE}.o"
# Executable.
EXE="`dirname ${LAT}`/a.out"

function error {
  echo >&2 "ERROR"
  cleanup
  exit 1
}

function cleanup {
  [ -e "${BC}" ] && rm "${BC}"
  [ -e "${OBJ}" ] && rm "${OBJ}"
}

./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/latc_llvm/latc_llvm ${LAT} || error
llvm-as ${LL} -o ${BC} || error
llvm-link ${BC} -o ${BC} ${RUNTIME} || error
llc -filetype=obj ${BC} -o ${OBJ} || error
gcc ${OBJ} -o ${EXE}
cleanup
