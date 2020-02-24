#include <benchmark/benchmark.h>
#include <string>
#include <algorithm>

const std::string soundOfSilence{"Hello darkness, my old friend\n"
                                 "I've come to talk with you again\n"
                                 "Because a vision softly creeping\n"
                                 "Left its seeds while I was sleeping\n"
                                 "And the vision that was planted in my brain\n"
                                 "Still remains"};
class DerefFixture : public benchmark::Fixture {
    public:
    void SetUp(const ::benchmark::State& state) { }
    void TearDown(const ::benchmark::State& state) { }
};

BENCHMARK_F(DerefFixture, dereferencePointer)(benchmark::State& state) {
    std::string* stringPtr = new std::string(soundOfSilence);
    bool found = false;
    for (auto _ : state) {
        if (stringPtr->find("softly") != std::string::npos)
            found = true;
    }
}

BENCHMARK_F(DerefFixture, useValue)(benchmark::State& state) {
    std::string string{soundOfSilence};
    bool found = false;
    for (auto _ : state) {
        if (string.find("softly") != std::string::npos)
            found = true;
    }
}

BENCHMARK_MAIN();
