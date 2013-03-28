all: Page

Page: *.hs
	ghc Page.hs

clean:
	rm -rf *.o *.hi

.PHONY: clean
