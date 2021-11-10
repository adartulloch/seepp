### MAKEFILE ####

parser.mly : 
		ocamllex parser.mly

# "clean" removes all generated files and output dumpfiles #
.PHONY : clean
clean :
		rm -rf parser.mli parser.ml scannerml *.output