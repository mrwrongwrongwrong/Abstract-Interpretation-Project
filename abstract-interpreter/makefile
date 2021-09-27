# file "makefile  © P. Cousot 2018"

.PHONY: all 
all: analyseTop analyseBotTop analyseParity analyseConst analyseSign

.PHONY: help 
help: 
	@echo "******************************************************************"	
	@echo "make analyseTop: {_|_}"
	@echo "make analyseBotTop: {_|_, T}"
	@echo "make analyseParity: parity analysis (for variables x,y only)"
	@echo "make analyseConst: consistency analysis (for variables x,y only)"
	@echo "make analyseCong: congruence analysis"
	@echo "make analyseSign: Abstract Sign analysis"
	@echo "******************************************************************"	

.PHONY: delete
delete: 
	@/bin/rm -f main *~ .*~ *.cmo *.cmi lexer.ml parser.ml parser.mli parser.conflicts
	@find . -lname '*.*' -delete

.PHONY: analyseTop analyseBotTop analyseParity analyseConst analyseSign
analyseTop : delete 
	@echo "**************************************************"
	@echo "*** top domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f abstractDomainTop.ml abstractDomain.ml
	@make --always-make main
	@make --always-make examples

analyseBotTop : delete 
	@echo "*** bot/top domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f abstractDomainBotTop.ml abstractDomain.ml
	@make --always-make main
	@make --always-make examples
	
analyseParity : delete 
	@echo "*** parity domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f abstractDomainParity.ml abstractDomain.ml
	@make --always-make main
	@make --always-make examples

analyseConst : delete 
	@echo "*** constancy domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f abstractDomainConstancy.ml abstractDomain.ml
	@make --always-make main
	@make --always-make examples

analyseSign : delete 
	@echo "*** sign domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f abstractDomainSign.ml abstractDomain.ml
	@make --always-make main
	@make --always-make examples

main: 
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlc -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlc abstractSyntaxExpressions.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

.PHONY: examples
.IGNORE: examples
examples: main
	@echo "*** using the forward abstract interpreter:"
	@echo "**************************************************"
	@echo "*** ;"
	@echo ";"| ./main
	@echo "**************************************************"
	@echo "*** x = 42;"
	@echo "x = 42;"| ./main
	@echo "**************************************************"
	@echo "*** break;"
	@echo "break;"| ./main
	@echo "**************************************************"
	@echo "*** break; x = 7;"
	@echo "break; x = 7;"| ./main
	@echo "**************************************************"
	@echo "*** x = 7; ; break; "
	@echo "x = 7; ; break; "| ./main
	@echo "**************************************************"
	@echo "*** {}" 
	@echo "{}" | ./main
	@echo "**************************************************"
	@echo "*** x=-10-20--40;"
	@echo "x=-10-20--40;" | ./main
	@echo "**************************************************"
	@echo "*** x=1; y=2;"
	@echo "x=1; y=2;" | ./main
	@echo "**************************************************"
	@echo "*** {x=10; ; y=20;}"
	@echo "{x=10; ; y=20;}" | ./main
	@echo "**************************************************"
	@echo "*** if (1-2<3-4-5) x=-x;"
	@echo "if (1-2<3-4-5) x=-x;" | ./main
	@echo "**************************************************"
	@echo "*** if (x<1) if (x<0) x=1; else if (x<0) { x=2; x=3; } else { x=4; x=5; x=6; }"
	@echo "if (x<1) if (x<0) x=1; else if (x<0) { x=2; x=3; } else { x=4; x=5; x=6; }" | ./main
	@echo "**************************************************"
	@echo "*** while (x<1) {}"
	@echo "while (x<1) {}" | ./main
	@echo "**************************************************"
	@echo "*** while (x<1) x = x + 1;"
	@echo "while (x<1) x = x + 1;" | ./main
	@echo "**************************************************"
	@echo "*** while (x<10) x = x + 1;"
	@echo "while (x<10) x = x + 1;" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1){x = x - 1;} x = 42;"
	@echo "while (0<1){x = x - 1;} x = 42;" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1){}"
	@echo "while (0<1){}" | ./main
	@echo "**************************************************"
	@echo "*** x=x-1;while (0<1){x=x-1;if(x<2)break;};"
	@echo "x=x-1;while (0<1){x=x-1;if(x<2)break;};" | ./main
	@echo "**************************************************"
	@echo "*** x=-10; while (x<0) if (x<0) if (0<x) x=-x;"
	@echo "x=-10; while (x<0) if (x<0) if (0<x) x=-x;" | ./main
	@echo "**************************************************"
	@echo "*** x=10; while (x<0) { x=x-1; break; }; x= 10;"
	@echo "x=10; while (x<0) { x=x-1; break; }; x= 10;" | ./main
	@echo "**************************************************"
	@echo "*** x=-10; while (x<0) { x=x-1; break; }; x= 10;"
	@echo "x=-10; while (x<0) { x=x-1; break; }; x= 10;" | ./main
	@echo "**************************************************"
	@echo "*** x=0; while (x<0) { while (x<0) x=x-1; x= 10; }; x= 100;"
	@echo "x=0; while (x<0) { while (x<0) x=x-1; x= 10; }; x= 100;" | ./main
	@echo "**************************************************"
	@echo "*** x=0; while (x<0) { while (x<0) x=x-1; break; }; x= 100;"
	@echo "x=0; while (x<0) { while (x<0) x=x-1; break; }; x= 100;" | ./main
	@echo "**************************************************"
	@echo "*** x=x-1; while (0<1) { x=x-1; if (2 < x) break; };"
	@echo "x=x-1; while (0<1) { x=x-1; if (2 < x) break; };" | ./main
	@echo "**************************************************"
	@echo "*** x=10; while (x>0) x=x-1;"
	@echo "x=10; while (x>0) x=x-1;" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1){ break; x=1; }"
	@echo "while (0<1){ break; x=1; }" | ./main
	@echo "**************************************************"
	@echo "*** while (x!=2) { if (x==0) break; if (x==1) break; }"
	@echo "while (x!=2) { if (x==0) break; if (x==1) break; }" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1) x=x+2;"
	@echo "while (0<1) x=x+2;" |./main
	@echo "*** end"
	@echo "**************************************************"

.PHONY: clean
clean: 
	@/bin/rm -f main *~ .*~ *.cmo *.cmi abstractDomain.ml lexer.ml parser.ml parser.mli parser.conflicts
	@find . -lname '*.*' -delete

