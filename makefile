lab4: lab4d
	lex lab4.lex
	yacc lab4.yacc
	gcc y.tab.c lex.yy.c -o lab4

lab4d:	lab4.lex lab4.yacc
	lex -d lab4.lex
	yacc -d lab4.yacc
	gcc y.tab.c lex.yy.c -o lab4d

clean:
	rm -f y.tab.c y.tab.h lex.yy.c lab4d lab4

