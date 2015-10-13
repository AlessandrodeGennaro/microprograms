PROJECT_NAME="ISA Specification"

install:
	@echo "Compiling $(PROJECT_NAME)..."
	@ghc --make Main.hs -o simulator

run:
	@echo "Running simulator..."
	@./simulator

clean:
	@echo "Removing $(PROJECT_NAME)..."
	@rm *.hi *.o simulator
	@echo "$(PROJECT_NAME) removed correctly."

rmtmp:
	@rm *~
	@echo "Temporary files removed correctly."

help:
	@echo "\nHelp of $(PROJECT_NAME):"
	@echo "\tinstall - compiles and builds the project."
	@echo "\trun - runs the simulator."
	@echo "\tclean - removes all the compilation and building files."
	@echo "\trmtmp - removes temporary files."
	@echo "\thelp - print out the help of the tool.\n"
