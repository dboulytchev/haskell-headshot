all:
	ghc -o int int.hs

clean:
	rm -Rf *~ *.hi *.o int

