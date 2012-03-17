SHELL=zsh

all: dotfs

# hm, this should depend on all hs files...
dotfs: src/DotFS.hs src/Core/*.hs src/Util/*.hs
	ghc --make src/DotFS.hs -o $@ -threaded -isrc

lint:
	cabal clean
	clear; hlint -c **/*.hs

clean:
	cabal clean
	rm -vf dotfs **/*.hi **/*.o ;
	rm -vrf dist
