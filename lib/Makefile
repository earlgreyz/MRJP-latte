all: llvm
	llvm-as runtime.ll -o runtime.bc

llvm:
	clang -O0 -o runtime.ll -emit-llvm -S runtime.c

clean:
	rm runtime.ll runtime.bc
