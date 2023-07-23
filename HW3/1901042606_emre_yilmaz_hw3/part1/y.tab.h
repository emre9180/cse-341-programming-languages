/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    DEFFUN = 258,
    DEFV = 259,
    KW_WHILE = 260,
    KW_IF = 261,
    KW_EXIT = 262,
    KW_LOAD = 263,
    KW_DISP = 264,
    KW_TRUE = 265,
    KW_FALSE = 266,
    KW_NIL = 267,
    OP_PLUS = 268,
    OP_MINUS = 269,
    OP_EQ = 270,
    OP_DIV = 271,
    OP_MULT = 272,
    OP_CP = 273,
    OP_OP = 274,
    OP_COMMA = 275,
    OP_SET = 276,
    OP_AND = 277,
    OP_OR = 278,
    OP_NOT = 279,
    OP_GT = 280,
    COMMENT = 281,
    VALUEF = 282,
    ID = 283
  };
#endif
/* Tokens.  */
#define DEFFUN 258
#define DEFV 259
#define KW_WHILE 260
#define KW_IF 261
#define KW_EXIT 262
#define KW_LOAD 263
#define KW_DISP 264
#define KW_TRUE 265
#define KW_FALSE 266
#define KW_NIL 267
#define OP_PLUS 268
#define OP_MINUS 269
#define OP_EQ 270
#define OP_DIV 271
#define OP_MULT 272
#define OP_CP 273
#define OP_OP 274
#define OP_COMMA 275
#define OP_SET 276
#define OP_AND 277
#define OP_OR 278
#define OP_NOT 279
#define OP_GT 280
#define COMMENT 281
#define VALUEF 282
#define ID 283

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 113 "parser.y"

    int value;
    char id[50];
    float valuef;

#line 119 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
