(* User  declarations *)

%%
(* required declarations *)
%name a2

%term
  CONST of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | ID of string | TERM | EOF

%nonterm program of string option| statement of string | formula of string | statement_list of string

%pos int

(*optional declarations *)             
%eop EOF
%noshift EOF

(* %header  *)


%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT


%start program

%verbose

%%
program: statement_list(SOME(statement_list^"program: statement_list "))

statement_list: statement_list statement(statement_list^statement^"statement_list: statement_list statement ,")              
        | ("")

statement: formula TERM(formula1 ^ "TERM ';' ," ^ "statement: formula TERM ,")    

formula: NOT formula("NOT 'NOT' ," ^ formula1 ^ "formula : NOT formula ,")
      |  formula AND formula(formula1 ^ "AND 'AND' ," ^ formula2 ^ "formula : formula AND formula ," )
      |  formula OR formula(formula1 ^ "OR 'OR' ," ^ formula2 ^ "formula : formula OR formula ,")
      |  formula XOR formula(formula1 ^ "XOR 'XOR' ," ^ formula2 ^ "formula : formula XOR formula ," )
      |  formula EQUALS formula(formula1 ^ "EQUALS 'EQUALS' ," ^ formula2 ^ "formula : formula XOR formula ,")           
      |  formula IMPLIES formula(formula1 ^ "IMPLIES 'IMPLIES' ," ^ formula2 ^ "formula : formula IMPLIES formula ,")                     
      |  IF formula THEN formula ELSE formula("IF 'IF' ," ^ formula1 ^ "THEN 'THEN' ," ^ formula2 ^ "ELSE 'ELSE' ," ^ formula3 ^ "formula : IF formula THEN formula ELSE formula ,")  
      |  LPAREN formula RPAREN("LPAREN '(' ," ^ formula1 ^ "RPAREN ')' ," ^ "formula : LPAREN formula RPAREN ,")        
      |  ID("ID '" ^ ID ^ "' ," ^ "formula : ID ,")                               
      |  CONST("CONST '" ^ CONST ^ "' ," ^ "formula : CONST ,")        