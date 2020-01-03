#!/bin/bash

COMPILER="${BASH_SOURCE%/*}/../latc_llvm"

for f in ${1}/*.lat; do
	echo -n >&2 "Testing ${f}... ";
	${COMPILER} ${f}
	IN=${f%.lat}.input
	OUT=/tmp/lat.output
	EXEC="`dirname ${f}`/a.out"
	if [ -f ${IN} ]; then
		${EXEC} < ${IN} > ${OUT}
	else
		${EXEC} > ${OUT}
	fi
	rm ${EXEC}
	diff -q ${OUT} "${f%.lat}.output"
done
