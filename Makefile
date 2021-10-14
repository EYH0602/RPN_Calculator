CC = ghc

all: main

main: main.hs
	$(CC) main.hs

clean:
	rm *.hi *.o main