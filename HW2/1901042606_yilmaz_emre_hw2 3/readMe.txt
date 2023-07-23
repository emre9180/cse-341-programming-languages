Emre Yilmaz, 1901042606, Gebze Technical University

IMPORTANT NOTE for 2nd part: The list outputs includes 1 line. If it will be necessary, in the next homework, the lines are going to merge. 


NOTES FOR T.A.

This is how to run the first part ( flex ):
	lex flex.l
	gcc lex.yy.c
	./a.out or ./a.out <test.txt
	
If you want to test the program with a file, you must add the end of the run command as you see above. If you want to enter the instances yourself, you must no enter the file name.


For 2nd part:
	clisp lexer.lisp test.txt
	This command reads the text and tokenise, then it waits for input from you. If you only want to try yourself, do not enter a file name. In this case, the flex waits for you and an input


	
