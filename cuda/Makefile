

cpu: cpuAdd.cpp
	g++  cpuAdd.cpp -o cpuAdd.bin

gpu: gpuAdd.cu
	nvcc  gpuAdd.cu -o gpuAdd.bin

all: cpu gpu

profileCpu: cpu
	time ./cpuAdd.bin
profileGpu: gpu
	nvprof --unified-memory-profiling off ./gpuAdd.bin

clean:
	rm cpuAdd.bin
	rm gpuAdd.bin