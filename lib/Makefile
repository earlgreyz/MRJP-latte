all: llvm
	llvm-as runtime.ll -o runtime.bc

llvm:
	clang -O0 -o runtime.ll -emit-llvm -S runtime.c

.PHONY: clean
clean:
	rm -f runtime.ll runtime.bc
