out = genblog

all:
	ghc -Wall -o $(out) Main.hs

dynamic:
	ghc -dynamic -Wall -o $(out) Main.hs

.PHONY: clean
clean:
	find . \( -iname "*.hi" -o -iname "*.o" \) -exec rm {} \;
