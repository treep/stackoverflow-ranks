
all:
	ghc -Wall -fno-warn-orphans -O2 -threaded -rtsopts ranks.hs -o ranks

clean:
	rm -f *.hi *.o ranks
