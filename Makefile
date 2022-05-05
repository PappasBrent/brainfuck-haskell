main : main.hs
	ghc main.hs

clean:
	rm -fr *.hi *.o main

test: main
	./main test