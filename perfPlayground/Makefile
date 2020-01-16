all: testDriver

INCLUDE_CMDS := -isystem thirdParty/benchmark/include
LINK_CMDS := -LthirdParty/benchmark/build/src -lbenchmark -lpthread

testDriver: test.cpp
	g++ test.cpp -std=c++17 $(INCLUDE_CMDS) $(LINK_CMDS) -o test

clean:
	rm test