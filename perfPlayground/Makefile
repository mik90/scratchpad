all: stringTest

INCLUDE_CMDS := -isystem thirdParty/benchmark/include
LINK_CMDS := -LthirdParty/benchmark/build/src -lbenchmark -lpthread

stringTest: stringTest.cpp
	g++ stringTest.cpp -std=c++17 $(INCLUDE_CMDS) $(LINK_CMDS) -o driverStringTest

clean:
	rm driverStringTest
