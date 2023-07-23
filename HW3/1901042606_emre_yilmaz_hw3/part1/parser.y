%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    void yyerror (char *s);
int yylex();



struct idvalue
{
        char id[50];
        float valuef;
};

struct repo
{
        struct idvalue *pairs;
        int number;
};

struct repo *variables;
struct repo *funcs;

void startRepo()
{
        variables = (struct repo*)malloc(sizeof(struct repo));
        funcs = (struct repo*)malloc(sizeof(struct repo));
}

int checkIdExist(char id[50])
{
        int i;
        for(i=0;i<variables->number;i++)
        {
                if(strcmp(id, variables->pairs[i].id)==0)
                        return i;
        }

        return -1;
}

int checkFuncExist(char id[50])
{
        int i;
        for(i=0;i<funcs->number;i++)
        {
                if(strcmp(id, funcs->pairs[i].id)==0)
                        return i;
        }

        return -1;
}

float valueOfId(char id[50])
{
        int i;
        for(i=0;i<variables->number;i++)
        {
                if(strcmp(id, variables->pairs[i].id)==0)
                        variables->pairs[i].valuef;
        }
}

void addRepo(float value, char id[50])
{
        struct idvalue *pairs;

        pairs = (struct idvalue*)calloc(variables->number + 1, sizeof(struct idvalue));

        int i;
        for(i=0;i<variables->number;i++)
        {
                strcpy(pairs[i].id, variables->pairs[i].id);
                pairs[i].valuef = variables->pairs[i].valuef;
        }

        strcpy(pairs[i].id, id);
        pairs[i].valuef = value;

        variables->pairs = pairs;
        variables->number += 1;
}

void addRepoFuncs(char id[50])
{
        struct idvalue *pairs;

        pairs = (struct idvalue*)calloc(funcs->number + 1, sizeof(struct idvalue));

        int i;
        for(i=0;i<funcs->number;i++)
        {
                strcpy(pairs[i].id, funcs->pairs[i].id);
                pairs[i].valuef = funcs->pairs[i].valuef;
        }

        strcpy(pairs[i].id, id);
        pairs[i].valuef = 1;

        funcs->pairs = pairs;
        funcs->number += 1;
}

void updateRepo(float value, char id[50], int index)
{
        strcpy(variables->pairs[index].id, id);
        variables->pairs[index].valuef = value;
}
%}

%union{
    int value;
    char id[50];
    float valuef;
}

%start INPUT
%token DEFFUN
%token DEFV
%token KW_WHILE
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token KW_NIL

%token OP_PLUS
%token OP_MINUS
%token OP_EQ
%token OP_DIV
%token OP_MULT
%token OP_CP
%token OP_OP
%token OP_COMMA
%token OP_SET
%token OP_AND
%token OP_OR
%token OP_NOT
%token OP_GT

%token COMMENT
%token <valuef> VALUEF
%token <id> ID

%type <valuef> EXP
%type <valuef> EXPS
%type <valuef> EXPLIST
%type <valuef> ASG
%type <valuef> FUNCTION
%type <valuef> FCALL
%type <valuef> EXPB



%%

INPUT:
        FUNCTION {printf("Syntax OK.\nResult: %f\n",$1); } |
        EXP {printf("Syntax OK.\nResult: %f\n",$1); } |
        EXPLIST {printf("Syntax OK.\nResult: %f\n",$1); }

EXP:
        OP_OP OP_PLUS EXP EXP OP_CP {$$ = $3 + $4;} |
        OP_OP OP_MINUS EXP EXP OP_CP {$$ = $3 - $4;}|
        OP_OP OP_MULT EXP EXP OP_CP {$$ = $3 * $4;}|
        OP_OP OP_DIV EXP EXP OP_CP {$$ = $3 / $4;}|
        ID
        {
        int index = checkIdExist($1);

        if(index!=-1)
        {
                struct idvalue target = variables->pairs[index];
                $$ = target.valuef;
        }
        else
        {
                printf("No identifier named %s", $1);
                exit(1);
        }
        
        } | 
        VALUEF {$$ = $1;}| 
        FCALL {$$ = $1;}| 
        ASG {$$ = $1;} |
        OP_OP KW_IF EXPB EXPLIST EXPLIST OP_CP {$$ = (1 == $3) ? $4: $5;} | 
        OP_OP KW_WHILE EXPB EXPLIST OP_CP {$$ = (1 == $3) ? $4 : 0;}

EXPLIST: // deistirdim
        OP_OP EXPS OP_CP {$$ = $2;}

EXPS:
        EXP {$$ = $1;} |
        EXP EXPS {$$ = $2;}



ASG:
        OP_OP OP_SET ID EXP OP_CP
        {
                $$ = $4;
                int index = checkIdExist($3);
                if(index!=-1)
                        updateRepo($4, $3, index);
                else
                        addRepo($4, $3);
        }

FUNCTION:
        OP_OP DEFFUN ID OP_OP OP_CP OP_OP EXPLIST OP_CP{addRepoFuncs($3); $$ = 0;} | // sil
        OP_OP DEFFUN ID OP_OP ID OP_CP OP_OP EXPLIST OP_CP{addRepoFuncs($3); $$ = 0;}|
        OP_OP DEFFUN ID OP_OP ID ID OP_CP OP_OP EXPLIST OP_CP{addRepoFuncs($3); $$ = 0;}|
        OP_OP DEFFUN ID OP_OP ID ID ID OP_CP OP_OP EXPLIST OP_CP{addRepoFuncs($3); $$ = 0;}

FCALL: 
        OP_OP ID OP_CP 
        {
        if(checkFuncExist($2)==-1)
        {
                printf("There is no function named %s", $2);
                exit(1);
        }
        
        $$ = 1;
        } |

        OP_OP ID EXP OP_CP {if(checkFuncExist($2)==-1)
        {
                printf("There is no function named %s", $2);
                exit(1);
        }
        $$ = $3;} |

        OP_OP ID EXP EXP OP_CP {if(checkFuncExist($2)==-1)
        {
                printf("There is no function named %s", $2);
                exit(1);
        }
        $$ = $4;} |

        OP_OP ID EXP EXP EXP OP_CP {if(checkFuncExist($2)==-1)
        {
                printf("There is no function named %s", $2);
                exit(1);
        }
        $$ = $5;}

EXPB:
        OP_OP OP_EQ EXP EXP OP_CP {$$ = ($3 == $4);} |
        OP_OP OP_GT EXP EXP OP_CP {$$ = ($3 > $4);} |
        KW_TRUE {$$ = 1;} |
        KW_FALSE {$$ = 0;} |
        OP_OP OP_AND EXPB EXPB OP_CP {$$ = ($3 && $4);} |
        OP_OP OP_OR EXPB EXPB OP_CP {$$ = ($3 || $4);} |
        OP_OP OP_NOT EXPB OP_CP {$$ = !($3);}

%%
        


int main(int argc, char **argv)
{       
        startRepo();
         while(1){
                printf("Enter line: ");
                        yyparse();
    }
}