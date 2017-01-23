ghc=/opt/ghc/7.10.2/bin/ghc

all:
	$(ghc) -o int int.hs
	$(ghc) -o analyze analyze.hs

clean:
	rm -Rf *~ *.hi *.o int analyze

