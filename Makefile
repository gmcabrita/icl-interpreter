src = Main.hs Parser.hs Lexer.hs Syntax.hs

all: $(src)
	ghc -w --make Main.hs -o main

Parser.hs: Parser.y
	happy --ghc $<

Lexer.hs: Lexer.x
	alex --ghc $<

clean:
	rm -rf main *.hi *.o Lexer.hs Parser.hs
