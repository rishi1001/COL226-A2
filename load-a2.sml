structure a2LrVals = a2LrValsFun(structure Token = LrParser.Token)
structure a2Lex = a2LexFun(structure Tokens = a2LrVals.Tokens);
structure a2Parser =            
	  Join(structure LrParser = LrParser
     	       structure ParserData = a2LrVals.ParserData
     	       structure Lex = a2Lex)
     
fun invoke lexstream =                               
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    a2Parser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =                          

    let val done = ref false
    	val lexer=  a2Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	    lexer
    end	
		
fun parse (lexer) =            
    let val dummyEOF = a2LrVals.Tokens.EOF(0,0)
        val pobracket = print("[")
    	val (result, lexer) = invoke lexer
        val pcbracker = print("]\n")
	val (nextToken, lexer) = a2Parser.Stream.get lexer
    in
        if a2Parser.sameToken(nextToken, dummyEOF) 
        then (
            case result of SOME c => print("[" ^ c ^ "]\n")
                        |  NONE   => print("")
            )
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); 
            case result of SOME c => print("[" ^ c ^ "]\n")
                        |  NONE   => print("")
            )
    end

fun a2 (infilename) = 
    let
        val inf = TextIO.openIn infilename
        val s = TextIO.input inf
    in
        parse(stringToLexer(s))
    end
