SHELL=zsh

all:  funion

funion: Funion.hs
	ghc --make $< -o $@ -threaded

lint:
	clear; hlint -c **/*.hs | less

clean:
	rm -vf funion **/*.hi **/*.o ;
	rm -vrf dist
