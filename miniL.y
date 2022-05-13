/* calculator. */
%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char *msg);
extern int currLine;
extern int currPos;
FILE * yyin;
%}

%union{
  /* put your types here */
  double dval;
  char* sval;
}

%error-verbose
%start input
%token END FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ELSE FOR WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN MOD MULT DIV PLUS MINUS EQUAL L_PAREN R_PAREN ENDIF EQ NEQ LT GT LTE GTE SEMICOLON COLON COMMA L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN
%token <sval> IDENT
%token <dval> NUMBER
%right ASSIGN
%left OR
%left AND
%right NOT
%left EQ NEQ LT GT LTE GTE
%left  ADD SUB
%left MULT DIV MOD
%right UMINUS
%left L_SQUARE_BRACKET R_SQUARE_BRACKET
%left L_PAREN R_PAREN 

%% 
input:	
			| input program
			;

program:	var END {printf("program -> var \n");}         
			;

var:
      ident {printf("var -> ident \n");}
      | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
   ;

ident:
      IDENT {printf("ident -> IDENT %s\n", $1);}
   ;

expression:
      NUMBER {printf("%f\n", $1);}
    ;

%% 

int main(int argc, char **argv) {
   if (argc > 1) {
      yyin = fopen(argv[1], "r");
      if (yyin == NULL){
         printf("syntax: %s filename\n", argv[0]);
      }//end if
   }//end if
   yyparse(); // Calls yylex() for tokens.
   return 0;
}
void yyerror(const char *msg) {
 printf("** Line %d, position %d: %s\n", currLine, currPos, msg);
}
