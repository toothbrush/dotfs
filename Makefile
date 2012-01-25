SHELL=zsh

all: dotfs

# hm, this should depend on all hs files...
dotfs: DotFS.hs Core/*.hs Util/*.hs
	ghc --make DotFS.hs -o $@ -threaded

lint:
	clear; hlint -c **/*.hs | less

clean:
	rm -vf dotfs **/*.hi **/*.o ;
	rm -vrf dist
