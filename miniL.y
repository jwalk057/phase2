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

program:	statement END {printf("program -> statement \n");}         
			;

statement:
        var ASSIGN expression {printf("statement -> var ASSIGN expression \n");}
        | IF bool-expr THEN statement SEMICOLON ENDIF {printf("statement -> IF bool-expr THEN statement SEMICOLON ENDIF \n");}
        | IF bool-expr THEN statement SEMICOLON another-if-statement ENDIF {printf("statement -> IF bool-expr THEN statement SEMICOLON another-if-statement ENDIF \n");}
        | IF bool-expr THEN statement SEMICOLON ELSE statement SEMICOLON ENDIF {printf("statement -> IF bool-expr THEN statement SEMICOLON ELSE statement SEMICOLON ENDIF \n");}
        | IF bool-expr THEN statement SEMICOLON another-if-statement ELSE statement SEMICOLON ENDIF {printf("statement -> IF bool-expr THEN statement SEMICOLON another-if-statement ELSE statement SEMICOLON ENDIF \n");}
        | IF bool-expr THEN statement SEMICOLON another-if-statement ELSE statement SEMICOLON another-else-statement ENDIF {printf("statement -> IF bool-expr THEN statement SEMICOLON another-if-statement ELSE statement SEMICOLON another-else-statement ENDIF \n");}
        | WHILE bool-expr BEGINLOOP statement SEMICOLON ENDLOOP {printf("statement -> WHILE bool-expr BEGINLOOP statement SEMICOLON ENDLOOP \n");}
        | WHILE bool-expr BEGINLOOP statement SEMICOLON another-statement ENDLOOP {printf("statement -> WHILE bool-expr BEGINLOOP statement SEMICOLON another-statement ENDLOOP \n");}
        | DO BEGINLOOP statement SEMICOLON ENDLOOP WHILE bool-expr {printf("statement -> DO BEGINLOOP statement SEMICOLON ENDLOOP WHILE bool-expr \n");}
        | DO BEGINLOOP statement SEMICOLON another-statement ENDLOOP WHILE bool-expr {printf("statement -> DO BEGINLOOP statement SEMICOLON another-statement ENDLOOP WHILE bool-expr \n");}
        | READ var {printf("statement -> READ var \n");}
        | READ var COMMA another-var {printf("statement ->  READ var COMMA another-var \n");}
        | WRITE var {printf("statement -> WRITE var \n");}
        | WRITE var COMMA another-var {printf("statement -> WRITE var COMMA another-var \n");}
        | CONTINUE {printf("statement -> CONTINUE \n");}
        | RETURN expression {printf("statement -> RETURN expression \n");}
      ;

another-var:
         var {printf("another-var -> var \n");}
         | var COMMA another-var {printf("another-var -> var COMMA another-var \n");}
      ;

another-statement:
        | statement SEMICOLON another-statement {printf("another-statement  -> statement SEMICOLON another-statement \n");}
      ;

another-if-statement:
        | statement SEMICOLON another-if-statement {printf("another-if-statement  -> statement SEMICOLON another-if-statement \n");}
      ;

another-else-statement:
        | statement SEMICOLON another-else-statement {printf("another-else-statement  -> statement SEMICOLON another-else-statement \n");}
      ;

bool-expr:
        relation-and-expr {printf("bool-expr -> relation-and-expr \n");}
        | relation-and-expr OR bool-expr {printf("bool-expr -> relation-and-expr OR bool-expr \n");}
      ;

relation-and-expr:
          relation-expr {printf("relation-and-expr -> relation-expr \n");}
          |relation-expr AND relation-and-expr {printf("relation-and-expr -> relation-expr AND relation-and-expr \n");}
      ;

relation-expr:
        expression comp expression {printf("relation-expr -> expression comp expression \n");}
        | TRUE {printf("relation-expr -> TRUE \n");}
        | FALSE {printf("relation-expr -> FALSE \n");}
        | L_PAREN bool-expr R_PAREN {printf("relation-expr -> L_PAREN bool-expr R_PAREN \n");}
        | NOT expression comp expression {printf("relation-expr -> NOT expression comp expression \n");}
        | NOT TRUE {printf("relation-expr -> NOT TRUE \n");}
        | NOT FALSE {printf("relation-expr -> NOT FALSE \n");}
        | NOT L_PAREN bool-expr R_PAREN {printf("relation-expr -> NOT L_PAREN bool-expr R_PAREN \n");}
      ;

comp:
      EQ {printf("comp -> EQ \n");}
      |NEQ {printf("comp -> NEQ \n");}
      | LT {printf("comp -> LT \n");}
      | GT {printf("comp -> GT \n");}
      | LTE {printf("comp -> LTE \n");}
      | GTE {printf(" comp -> GTE \n");}
      ;

term:
      SUB var {printf("term -> SUB var \n");}
      | SUB NUMBER  {printf("term -> SUB NUMBER \n");}
      | SUB L_PAREN expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN \n");}
      | var {printf("term -> var \n");}
      | NUMBER {printf("term -> NUMBER %f \n", $1);}
      | L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN \n");}
      | ident L_PAREN R_PAREN {printf(" term -> ident L_PAREN R_PAREN");}
      | ident L_PAREN expression R_PAREN {printf("term -> ident L_PAREN expression R_PAREN \n");}
      | ident L_PAREN expression another-expression R_PAREN {printf("term -> ident L_PAREN expression another-expression R_PAREN \n");}
    ;

another-expression:
      | COMMA expression another-expression {printf("another-expression -> COMMA expression another-expression \n");}
    ;

expression:
      multipicative-expr {printf("expression -> multipicative-expr \n");}
      | multipicative-expr ADD expression {printf("expression -> multipicative-expr ADD expression \n");}
      | multipicative-expr SUB expression {printf("expression -> multipicative-expr SUB expression \n");}
    ;

multipicative-expr:
          term {printf("multipicative-expr -> term \n");}
          | term MULT multipicative-expr {printf("multipicative-expr -> term MULT multipicative-expr \n");}
          | term DIV multipicative-expr {printf("multipicative-expr -> term DIV multipicative-expr \n");}
          | term MOD multipicative-expr {printf("multipicative-expr -> term MOD multipicative-expr \n");}
;

var:
      ident {printf("var -> ident \n");}
      | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
    ;
  
ident:
      IDENT {printf("ident -> IDENT %s\n", $1);}
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
