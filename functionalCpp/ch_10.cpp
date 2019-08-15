#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <variant>
#include "../../range-v3/include/range/v3/all.hpp"

// 10. Monads
// 10.1
// C++ functor is an object that acts like a function
// Functional programming uses the word differently
// A class template F is a functor if it has a transform (map)
// function defined on it

// transform function for std::optional
template <typename T1, typename F>
auto transform(const std::optional<T1>& opt, F f)
  ->decltype(std::make_optional(f(opt.value())))
{
  if (opt)
    // If value, transform it with f()
    return std::make_optional(f(opt.value()));
  else
    return {}; // Return nothing
}

// 10.2
// Without addressing it, returning optionals ends up with
// nested optionals such as optional<optional<string>>
// Nested functors can be a problem.
// Monads address this
//
// A monad m<T> is a functor with an additional function defined
// in it that removes a level of nesting.
//  join: M<M<T>> -> M<T>
// Join allows you to chain transformations that can fail
// This can be simplified by binding (call it mbind)
// which is a composition of transform and join.
// A monad is something you can construct and mbind to a function

// 10.3
// Create a functor from std::vector
template <typename T, typename F>
auto transform(const std::vector<T>& xs, F f)
{
  // Transform vector xs with function of type F, f
  return xs | view::transform(f) | to_vector;
}
// Construct vector from a single value
template <typename T>
std::vector<T> make_vector(T&& value)
{
  return {std::forward<T>(value)};
}

// Creating these temporaries is inefficient, but this is
// done as an example
template <typename T, typename F>
auto mbind(const std::vector<T>& xs, F f)
{
  // Calls f which gives us a range of vectors.
  // Convert that range to a vector
  auto transformed = xs | view::transform(f)
                        | to_vector;
  // Convert the vector of vectors to a single vector
  return transformed | view::join | to_vector;
}

// 10.4 Range and monad comprehensions
// Create a filtering function:
template <typename C, typename P>
auto filter(const C& collection, P predicate)
{
  // Runs the lambda on each element in the collection
  return collection | mbind([=] (auto element)
      {
        // Creates range with either 1 or 0 elements
        return view::single(element) 
                | view::take(predicate(element) ? 1 : 0);
      }
}

// A range comprehension has two components:
// for_each - traverses a collection and collects all the values
// yielded from the function it was given
// With multiple nested comprehensions, the yielded values are
// places consecutively so it's all flattened out.
// yield_if - puts a value in the resulting range if the predicate
// is satisfied
// 
// These are also called monad comprehensions

// 10.5 failure handling
// Use std::optional as a monad
//
// Use mbind to strip off the context and transform opt using
// function F f. It will return either
// the underlying, transformed, object or nothing.
//
// These can be chained together where opt will keep being
// transformed as long as it is a valid value. At the end
// it will either be valid or empty.
template <typename T, typename F>
auto mbind(const std::optional<T>& opt, F f) -> decltype(f(opt.value()))
{
  if (opt)
    return f(opt.value());
  else
    return {};
}

/* e.g.
 *
 * std::optional<std::string> current_user_html()
 * {
 *  return mbind(
 *          mbind(current_login, user_full_name),
 *          to_html);
 * }
 */
// expected<T,E> tells you what the error is

// F can return whatever type it wants
//
// This will keep call f on exp unless there's an error
template <
  typename T,typename E, typename F,
  typename Ret = typename std::result_of<F(T)>::type
    >
Ret mbind(const expected<T, E>& exp, F f)
{
  // Return the error from expected if there is one
  if (!exp)
    return Ret::error(exp.error());

  // Otherwise, call f on the value of expected
  return f(exp.value());
}

// The try monad

int main(int argc, char** argv)
{
  return 0;
}
