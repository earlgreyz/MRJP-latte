#!/bin/bash

for f in ${1}/*.lat; do
	echo -n >&2 "Testing ${f}... ";
	./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/latc_x86/latc_x86 $f > /tmp/lat.ll
	llvm-as /tmp/lat.ll -o /tmp/lat.bc
	llvm-link /tmp/lat.bc -o /tmp/lat.bc ./lib/runtime.bc
	IN=${f%.lat}.input
	OUT=/tmp/lat.output
	if [ -f ${IN} ]; then
		lli /tmp/lat.bc < ${IN} > ${OUT}
	else
		lli /tmp/lat.bc > ${OUT}
	fi
	diff -q ${OUT} "${f%.lat}.output"
done
