#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <variant>

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
//


int main(int argc, char** argv)
{
  return 0;
}
