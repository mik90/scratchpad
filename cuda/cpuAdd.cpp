#include <iostream>
#include <cmath>
#include <array>
#include <algorithm>

constexpr std::size_t nElements = 1000000; // 1 Million


void add(std::array<float, nElements>& ar1, std::array<float, nElements>& ar2) {
    for (unsigned i = 0; i < nElements; i++)
        ar2[i] += ar1[i];
}

int main() {
    std::array<float, nElements> ar1{};
    std::fill(ar1.begin(), ar1.end(), 1.0f);
   
    std::array<float, nElements> ar2{};
    std::fill(ar2.begin(), ar2.end(), 2.0f);

    add(ar1, ar2);

    float maxError = 0.0f;
    for (int i = 0; i < nElements; i++)
        maxError = std::fmax(maxError, std::fabs(ar2[i] - 3.0f));

    std::cout << "Max error:" << maxError << std::endl;

    return 0;
}