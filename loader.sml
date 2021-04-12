CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "a2.yacc.sig";
use "a2.yacc.sml";
use "a2.lex.sml";
use "load-a2.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)