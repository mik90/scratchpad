#include <benchmark/benchmark.h>
#include <string>

static void createAndCopy(benchmark::State& state) {
    for (auto _ : state) {
        std::string x = "hello";
        std::string copy(x);
    }
}


static void createAndMove(benchmark::State& state) {
    for (auto _ : state) {
        std::string x = "hello";
        std::string move(std::move(x));
    }
}

BENCHMARK(createAndCopy);
BENCHMARK(createAndMove);


const char* soundOfSilence = "Hello darkness, my old friend\n"
                             "I've come to talk with you again\n"
                             "Because a vision softly creeping\n"
                             "Left its seeds while I was sleeping\n"
                             "And the vision that was planted in my brain\n"
                             "Still remains";

static void createAndCopyBig(benchmark::State& state) {
    for (auto _ : state) {
        std::string x{soundOfSilence};
        std::string copy(x);
    }
}


static void createAndMoveBig(benchmark::State& state) {
    for (auto _ : state) {
        std::string x{soundOfSilence};
        std::string move(std::move(x));
    }
}

BENCHMARK(createAndCopyBig);
BENCHMARK(createAndMoveBig);



BENCHMARK_MAIN();
