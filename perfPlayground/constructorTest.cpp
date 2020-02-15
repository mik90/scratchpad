#include <benchmark/benchmark.h>
#include <string>

const char* soundOfSilence = "Hello darkness, my old friend\n"
                             "I've come to talk with you again\n"
                             "Because a vision softly creeping\n"
                             "Left its seeds while I was sleeping\n"
                             "And the vision that was planted in my brain\n"
                             "Still remains";

static void sso(benchmark::State& state) {
    std::string copyToMe;
    for (auto _ : state) {
        std::string x = "hello";
        copyToMe = x;
    }
}
BENCHMARK(sso);

static void smallStringUsingNew(benchmark::State& state) {
    std::string copyToMe;
    for (auto _ : state) {
        std::string* x = new std::string("hello");
        copyToMe = *x;
    }
}
BENCHMARK(smallStringUsingNew);

static void longStringOnStack(benchmark::State& state) {
    std::string copyToMe;
    for (auto _ : state) {
        std::string x = soundOfSilence;
        copyToMe = x;
    }
}
BENCHMARK(longStringOnStack);

static void longStringUsingNew(benchmark::State& state) {
    std::string copyToMe;
    for (auto _ : state) {
        std::string* x = new std::string(soundOfSilence);
        copyToMe = *x;
    }
}
BENCHMARK(longStringUsingNew);

BENCHMARK_MAIN();
