exception incorrectinput;

structure Tokens= Tokens
  
  type pos = int
  type col = int
  type check = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 1
  val col = ref 1
  val check = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, c:int) => TextIO.output(TextIO.stdOut,"Unknown token:" ^ (Int.toString l) ^  ":" ^ (Int.toString c) ^ ":" ^ e ^ "\n")

  
%%
%header (functor a2LexFun(structure Tokens:a2_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; col := 0; lex());
{ws}+    => (col := (!col) + 1;lex());

"TRUE"      => (if (!check)=0 then print("CONST \"TRUE\"") else print(", CONST \"TRUE\"");check:=1;col := (!col) + 4;Tokens.CONST(yytext,!pos,!pos));
"FALSE"      => (if (!check)=0 then print("CONST \"FALSE\"") else print(", CONST \"FALSE\"");check:=1;col := (!col) + 5;Tokens.CONST(yytext,!pos,!pos));
"NOT"      => (if (!check)=0 then print("NOT \"NOT\"") else print(", NOT \"NOT\"");check:=1;col := (!col) + 3;Tokens.NOT(!pos,!pos));
"AND"      => (if (!check)=0 then print("AND \"AND\"") else print(", AND \"AND\"");check:=1;col := (!col) + 3;Tokens.AND(!pos,!pos));
"OR"      => (if (!check)=0 then print("OR \"OR\"") else print(", OR \"OR\"");check:=1;col := (!col) + 2;Tokens.OR(!pos,!pos));
"XOR"      => (if (!check)=0 then print("XOR \"XOR\"") else print(", XOR \"XOR\"");check:=1;col := (!col) + 3;Tokens.XOR(!pos,!pos));
"EQUALS"      => (if (!check)=0 then print("EQUALS \"EQUALS\"") else print(", EQUALS \"EQUALS\"");check:=1;col := (!col) + 6;Tokens.EQUALS(!pos,!pos));
"IMPLIES"      => (if (!check)=0 then print("IMPLIES \"IMPLIES\"") else print(", IMPLIES \"IMPLIES\"");check:=1;col := (!col) + 7;Tokens.IMPLIES(!pos,!pos));
"IF"      => (if (!check)=0 then print("IF \"IF\"") else print(", IF \"IF\"");check:=1;col := (!col) + 2;Tokens.IF(!pos,!pos));
"THEN"      => (if (!check)=0 then print("THEN \"THEN\"") else print(", THEN \"THEN\"");check:=1;col := (!col) + 4;Tokens.THEN(!pos,!pos));
"ELSE"      => (if (!check)=0 then print("ELSE \"ELSE\"") else print(", ELSE \"ELSE\"");check:=1;col := (!col) + 4;Tokens.ELSE(!pos,!pos));
"("      => (if (!check)=0 then print("LPAREN \"(\"") else print(", LPAREN \"(\"");check:=1;col := (!col) + 1;Tokens.LPAREN(!pos,!pos));
")"      => (if (!check)=0 then print("RPAREN \")\"") else print(", RPAREN \")\"");check:=1;col := (!col) + 1;Tokens.RPAREN(!pos,!pos));
{alpha}+ => (if (!check)=0 then print("ID \""^yytext^"\"") else print(", ID \""^yytext^"\"");check:=1;col := (!col) + size(yytext);Tokens.ID(yytext,!pos,!pos));
";"      => (if (!check)=0 then print("TERM \";\"") else print(", TERM \";\"");check:=1;col := (!col) + 1;Tokens.TERM(!pos,!pos));
.        => (error (yytext,!pos,!col);
             raise incorrectinput);

