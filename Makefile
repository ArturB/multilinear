#NeuroLekser Makefile build info

all: .\tools\NeuroLekser.x .\tools\NeuroParser.y stack.yaml Tensor.cabal
	alex -o .\app\Lekser.hs .\tools\NeuroLekser.x
	happy -o .\app\Parser.hs -a -g -c .\tools\NeuroParser.y
	stack build
	
