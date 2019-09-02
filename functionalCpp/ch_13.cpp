// Move as much logic to compile-time as possible to avoid run-time errors
// Make use of the type system to enforce units of measurements.
// User defined literals can help with this
// This means that mixing and matching units with cause compiler errors unless
// you've provided a conversion function

// Pure functions make unit tests a lot easier as they avoid the permutations
// of states

// Auto generating test cases prevents the programmer from making errors in writing
// unit tests. If the generation uses randomness, it is best to write out the
// seed used for the random number generator

// Property based testing: find properties that mean the result is found and then
// implement checks to assert them when the function under test is given randomly
// generated input data.

// Comparative testing
// If we are implementing a Bitmapped Vector Trie, we can compare actions on it
// with a vector as it is supposed to mimic that data structure. Do the same
// actions on both and compare the values of the structures against each other

// For testing concurrent monadic systems, you can strip out the I/O and system
// specific operations. The functional style allows for components to be easily
// replaced for development or testing. The sink function can be wrapped as it
// normally returns void. Wrapping it allows it to be mocked so an expected
// reply can be returned.

// Using a random function with a normal distribution (or any type of distribution)
// can allow you to tailor input to have certain values more often than others.
//
// Fuzzing also uses random data.

int main(int argc, char** argv)
{
  return 0;
}
