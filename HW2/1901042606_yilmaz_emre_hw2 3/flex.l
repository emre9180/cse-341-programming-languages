%{
   // Definitions
%}

%%
and {printf("%s >>>>>>>>> KW_AND\n", yytext);}
or {printf("%s >>>>>>>>> KW_OR\n", yytext);}
not {printf("%s >>>>>>>>> KW_NOT\n", yytext);}
equal {printf("%s >>>>>>>>> KW_EQUAL\n", yytext);}
less {printf("%s >>>>>>>>> KW_LESS\n", yytext);}
nil {printf("%s >>>>>>>>> KW_NIL\n", yytext);}
list {printf("%s >>>>>>>>> KW_LIST\n", yytext);}
append {printf("%s >>>>>>>>> KW_APPEND\n", yytext);}
concat {printf("%s >>>>>>>>> KW_CONCAT\n", yytext);}
set {printf("%s >>>>>>>>> KW_SET\n", yytext);}
deffun {printf("%s >>>>>>>>> KW_DEFFUN\n", yytext);}
for {printf("%s >>>>>>>>> KW_FOR\n", yytext);}
if {printf("%s >>>>>>>>> KW_IF\n", yytext);}
exit {printf("%s >>>>>>>>> KW_EXIT\n", yytext);}
load {printf("%s >>>>>>>>> KW_LOAD\n", yytext);}
disp {printf("%s >>>>>>>>> KW_DISP\n", yytext);}
true {printf("%s >>>>>>>>> KW_TRUE\n", yytext);}
false {printf("%s >>>>>>>>> KW_FALSE\n", yytext);}



"+" {printf("%s >>>>>>>>> OP_PLUS\n", yytext);}
"-" {printf("%s >>>>>>>>> OP_MINUS\n", yytext);}
"/" {printf("%s >>>>>>>>> OP_DIV\n", yytext);}

"**" {printf("%s >>>>>>>>> OP_DBLMULT\n" , yytext);}
"*" {printf("%s >>>>>>>>> OP_MULT\n", yytext);}
"(" {printf("%s >>>>>>>>> OP_OP\n", yytext);}
")" {printf("%s >>>>>>>>> OP_CP\n",  yytext);}
" " {}



"," {printf("%s >>>>>>>>> OP_COMMA\n", yytext);}



":;" {printf("%s >>>>>>>>> COMMENT\n" , yytext);}

[1-9][0-9]*[f][0-9]* {printf("%s >>>>>>>>> VALUEF\n", yytext);}
[1-9][0-9]* {printf("%s >>>>>>>>> VALUEI\n", yytext);}
[a-zA-Z_]+[a-zA-Z0-9_]* {printf("%s >>>>>>>>> IDENTIFIER\n", yytext);}
. {printf("%s >>>>>>>>> ERROR!\n",  yytext);return 0;}


\n {return 0;}
%%

int yywrap(){}

yylex();

int main(int arg1, char *argv[])
{   
    if(argv[1]!=NULL)
    {
        yyin = fopen(argv[1], "r+");
        yylex();
        fclose(yyin);
    }
    
    else
    {
        while(1) yylex(); 
    }

    return 0;
}