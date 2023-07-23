parser.out:	parser.c
		yacc -d parser.y
		lex -o flex.c flex.l
		cc -g flex.c y.tab.c -ll -o parser.out

clean:
		rm parser.out
