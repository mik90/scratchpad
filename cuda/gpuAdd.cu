#include <iostream>
#include <math.h>


__global__ void gpuAdd(int nElements, float* x, float* y) {
    int index = threadIdx.x;
    int stride = blockDim.x;

    for (int i = index; i < nElements; i += stride)
        y[i] = y[i] + x[i];
}

int main(int argc, char** argv) {
    constexpr int nElements = 1000000; // 1 Million
    float *x;
    cudaMallocManaged(&x, nElements * sizeof(float));
    float *y;
    cudaMallocManaged(&y, nElements * sizeof(float));
    for (int i = 0; i < nElements; i++) {
        x[i] = 1.0f;
        y[i] = 2.0f;
    }

    int blockSize = 256;
    int numBlocks = (nElements + blockSize - 1) / blockSize;
    gpuAdd<<<numBlocks, blockSize>>>(nElements, x, y);

    cudaDeviceSynchronize();

    float maxError = 0.0f;
    for (int i = 0; i < nElements; i++)
        maxError = std::fmax(maxError, std::fabs(y[i] - 3.0f));

    std::cout << "Max error:" << maxError << std::endl;
    
    cudaFree(x);
    cudaFree(y);
    return 0;
}