SHELL=zsh

all: funion

# hm, this should depend on all hs files...
funion: Funion.hs
	ghc --make $< -o $@ -threaded

lint:
	clear; hlint -c **/*.hs | less

clean:
	rm -vf funion **/*.hi **/*.o ;
	rm -vrf dist
