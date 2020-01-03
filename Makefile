all: runtime compiler wrapper

runtime:
	$(MAKE) -C lib

compiler:
	stack build
	cp ./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/latc_llvm/latc_llvm ./compiler

wrapper:
	cp ./scripts/wrapper.sh ./latc_llvm

.PHONY: clean
clean:
	$(MAKE) -C lib clean
	stack clean
	rm -f compiler
	rm -f latc_llvm
