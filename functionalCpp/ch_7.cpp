#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <vector>
#include <mutex>
#include <map>
#include <type_traits>
#include <range/v3/all.hpp>

// Actions mutate the original collection
// Views do not mutate the collection
// Sentinel - value to test if you're at the end
// Delimited range - don't know the end in advance. Such as
//    reading while a predicate is true or until a null terminator


using namespace ranges;

// Write out the top 10 items in a collection
template <typename Range>
void write_top_10(const Range& xs) {
  auto items =
    view::zip(xs, view::ints(1)) // Starts range with index 1
      | view::transform([] (const auto& pair) {
              // Print out the rank and the name
              return std::to_string(pair.second) + " "
              + pair.first;
              })
      | view::take(10);

  for (const auto& item : items) {
    std::cout << item << std::endl;
  }
}

std::string string_to_lower(const std::string& s) {
  return s | view::transform(tolower);
}
std::string string_only_alnum(const std::string& s) {
  return s | view::filter(isalnum);
}


int main()
{
  std::vector<std::string> words = {"hello", "hi", "world"};
  write_top_10(words);
  // Use ranges to calculate work frequencies
  words = 
    istream_range<std::string>(std::cin) // Tokenizes input
    | view::transform(string_to_lower) // Then make them lowercase
    | view::transform(string_only_alnum) // Then only alphanumeric
    | view::remove_if(&std::string::empty); // Then remove empty tokens
  const auto results = 
    words | view::group_by(std::equal_to<>())
          | view::transform([] (const auto& group) {
             const auto begin = std::begin(group);
             const auto end = std::end(group);
             const auto count = std::distance(begin,end);
             const auto word = *begin;
             // Get size of each group, return a pair with
             // the frequency and the word
             return std::make_pair(count, word);
             })
         | to_vector | action ::sort;
  for (auto value: results | view::reverse
                           | view::take(10)) {
    std::cout << value.first << " " << value.second << std::endl;
  }
  return 0;
}
