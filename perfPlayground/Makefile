all: stringTest loopTest

INCLUDE_CMDS := -isystem thirdParty/benchmark/include
LINK_CMDS := -LthirdParty/benchmark/build/src -lbenchmark -lpthread

# Try combinations of:
# The results are very odd
# -mtune=native, -march=native, -O2 -O3
OPT_LEVEL := -O3
STD := -std=c++17

loopTest: loopTest.cpp
	g++ loopTest.cpp     $(OPT_LEVEL) $(STD) $(INCLUDE_CMDS) $(LINK_CMDS) -o driverLoopTestGcc.bin
	clang++ loopTest.cpp $(OPT_LEVEL) $(STD) $(INCLUDE_CMDS) $(LINK_CMDS) -o driverLoopTestClang.bin
	./driverLoopTestGcc.bin
	./driverLoopTestClang.bin

stringTest: stringTest.cpp
	g++ stringTest.cpp     $(OPT_LEVEL) $(STD) $(INCLUDE_CMDS) $(LINK_CMDS) -o driverStringTestGcc.bin
	clang++ stringTest.cpp $(OPT_LEVEL) $(STD) $(INCLUDE_CMDS) $(LINK_CMDS) -o driverStringTestClang.bin
	./driverStringTestGcc.bin
	./driverStringTestClang.bin

clean:
	rm *.bin
