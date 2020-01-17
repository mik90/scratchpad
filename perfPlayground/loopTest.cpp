#include <benchmark/benchmark.h>
#include <random>
#include <numeric>
#include <algorithm>
#include <iostream>

std::mt19937 mt;

class LoopFixture : public benchmark::Fixture {
    public:
    void SetUp(const ::benchmark::State& state) {
        constexpr unsigned defaultSeed = 12;
        mt.seed(defaultSeed);
    }

    void TearDown(const ::benchmark::State& state) {
    }
};

BENCHMARK_F(LoopFixture, classicLoopPrefixUnsigned)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (unsigned int i = 0; i < v.size(); ++i) {
            v[i]++;
        }
    }

    v.clear();
}

BENCHMARK_F(LoopFixture, classicLoopPrefix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (int i = 0; i < v.size(); ++i) {
            v[i]++;
        }
    }

    v.clear();
}

BENCHMARK_F(LoopFixture, loopNotEqualsPrefix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (int i = 0; i != v.size(); ++i) {
            v[i]++;
        }
    }

    v.clear();
}


BENCHMARK_F(LoopFixture, classicLoopPostfix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (int i = 0; i < v.size(); i++) {
            v[i]++;
        }
    }

    v.clear();
}

BENCHMARK_F(LoopFixture, iteratorLoopPrefix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (auto it = v.begin(); it != v.end(); ++it) {
            (*it)++;
        }
    }

    v.clear();
}

BENCHMARK_F(LoopFixture, iteratorLoopPostfix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (auto it = v.begin(); it != v.end(); it++) {
            (*it)++;
        }
    }

    v.clear();
}

BENCHMARK_F(LoopFixture, iteratorLessThan)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        for (auto it = v.begin(); it < v.end(); it++) {
            (*it)++;
        }
    }

    v.clear();
}

BENCHMARK_F(LoopFixture, forEachPrefix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        std::for_each(v.begin(), v.end(), [](int& n) {++n;});
    }

    v.clear();
}



BENCHMARK_F(LoopFixture, forEachPostfix)(benchmark::State& state) {
    std::vector<int> v(1000);
    std::iota(v.begin(), v.end(), 0);
    std::shuffle(v.begin(), v.end(), mt);

    for (auto _ : state) {
        std::for_each(v.begin(), v.end(), [](int& n) {n++;});
    }

    v.clear();
}

BENCHMARK_MAIN();
