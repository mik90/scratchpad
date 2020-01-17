all: stringTest loopTest

INCLUDE_CMDS := -isystem thirdParty/benchmark/include
LINK_CMDS := -LthirdParty/benchmark/build/src -lbenchmark -lpthread

# Try combinations of:
# The results are very odd
# -mtune=native, -march=native, -O2 -O3

loopTest: loopTest.cpp
	g++ loopTest.cpp -O2 -std=c++17 $(INCLUDE_CMDS) $(LINK_CMDS) -o driverLoopTest.bin
	./driverLoopTest.bin

stringTest: stringTest.cpp
	g++ stringTest.cpp -std=c++17 $(INCLUDE_CMDS) $(LINK_CMDS) -o driverStringTest.bin
	./driverStringTest.bin

clean:
	rm *.bin
