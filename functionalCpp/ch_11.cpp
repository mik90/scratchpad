#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <variant>
//#include "../../libraries/range-v3/include/range/v3/all.hpp"

// Ch 11 tmeplate metaprogramming

// Sum up values of a collection without being given initial value
// like std::accumulate has

// Problem: assumes that T is default constructible
// template <typename T>
// using contained_type_t = decltype(*begin(T()));

// declval<T> pretends to create instance of T, even if it's not
// default constructible
template <typename T>
using contained_type_t = std::remove_cv_t<
                            std::remove_reference_t<
                              decltype(*begin(std::declval<T>()))>>;
// static_assert can help test these during compliation
static_assert(std::is_same<int, contained_type_t<std::vector<int>>>(),
              "this vector should contain integers");
template <typename C,
          typename R = contained_type_t<C>>
R sum_iterable(const C& collection)
{
  // find sum
  return std::accumulate(std::begin(collection), std::end(collection), R());
}

template <typename C,
          typename R = typename C::value_type>
R sum_collection(const C& collection)
{
  // find sum
  return std::accumulate(std::begin(collection), std::end(collection), R());
}

// Can get passed anything, but the compiler will be able to check if the
// argument is a valid type
template <typename...>
using void_t = void;

// It is much easier to just have the collection define a member ::value_type

// Check if collection has a value type
// Assumes a type doesn't have a nested value type
template <typename C, typename = void_t<>>
struct has_value_type
  : std::false_type {};

// Specialized case where it does
template <typename C>
struct has_value_type<C,void_t<typename C::value_type>>
  : std::true_type {};

// Problem, the collcetion may not be iterable
/*template <typename C>
auto sum(const C& collection)
{
  if constexpr (has_value_type<C>())
    return sum_collection(collection);
  else
    return sum_iterable(collection);
}
*/

template <typename C, typename = void_t<>>
struct is_iterable 
  : std::false_type {};

// The end may be a sentinel we don't care if it can be dereferenced
template <typename C>
struct is_iterable<C, void_t<decltype(*std::begin(std::declval<C>())),
                             decltype(std::end(std::declval<C>()))>>
  : std::true_type {};

template <typename C>
auto sum(const C& collection)
{
  if constexpr (has_value_type<C>())
    return sum_collection(collection);
  else if constexpr (is_iterable<C>())
    return sum_iterable(collection);
}



int main(int argc, char** argv)
{
  return 0;
}
