all: flp-fun

flp-fun: flp-fun.hs
	ghc -Wall -o flp-fun flp-fun.hs

clean:
	rm flp-fun flp-fun.o flp-fun.hi