#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <variant>
#include "../../libraries/range-v3/include/range/v3/all.hpp"

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
// Passes an exception_ptr if an exception is thrown 
template <typename F,
          typename Ret = typename std::result_of<F()>::type,
          typename Exp = expected<Ret, std::exception_ptr>
Exp mtry(F f)
{
  // f cannot have any arguments, to call it with arguments, a lambda can be
  // used as f
  try {
    return Exp::success(f());
  }
  catch (...) {
    return Exp::error(std::current_exception());
  }
}

// Handling state with monads
// Simplest way is to pass the state along with other arguments
// and have it return the new state
//
// Something along the lines of a debugging log which is passed
// through from function to function is often referred to as a
// Writer monad.
template <typename T>
class with_log {
  public:
    with_log(T value, std::string log = std::string) :
      m_value(value), m_log(log) {}
    T value() const { return m_value; }
    std::string log() const { return m_log; }
  private:
    T m_value;
    std::string m_log;
};

// Can be used by functions as:
// with_log<std::string> user_full_name(const std::string& login);
// with_log<std::string> to_html(const std::string& text);

// An mbind needs to be implemented for it
// It will take in an instance of with_log
tempalte <typename T,
          typename F,
          typename Ret = typename std::result_of<F(T)>::type>
Ret mbind(const with_log<T>& val, F f)
{
  // Transform the value with f(). Returns the result
  // of the transformation along with the log
  const auto result_with_log = f(val.value());
  // Return the result. The log should be concatenated with the
  // previous log.
  return Ret(result_with_log.value(), val.log() + result_with_log.log());
}

// Concurrency and the continuation monad
// 10.7
// Future: instead of waiting for data to arrive,
// a function can return a handler for data that 
// will be available sometime in the future
// future<T> is kind of like a container
// Future is a functor, can take function f and
// transform <T1> to <T2>
//
// Since these operations can take a while, they can
// return futures
// future<std::string> user_full_name(const std::string& login);
// future<std::string> to_html(const std::string& text);
// mbind can help deal with a future of a future
future<std::string> current_user_html()
{
  return current_user() | mbind(user_full_name)
                        | mbind(to_html);
}

// Functions passed to mbind are continuations. A monad dealing
// with a sequence of future values is called the continuation monad
// To be used with callbacks and signals, this function could be split
// into multiple ones. Every time an async operation is called, you'd need
// to create a new function to handle the result.

// Implementation of futures
// future can contain an exception if the operation failed.
// Only way to get the value is through the member function .get()
// which will block if the future isn't ready.
//
// Can add a .then() which can be used to define what shall be done
// with the value once it is available
//
// .then() takes a function whose argument is an already completed
// future object and whose return type is a new future object. 
//
// This makes the mbind function simple
template <typename T, typename F>
auto mbind(const future<T>& future, F f)
{
  return future.then([] (future<T> finished)
      {
        // Doesn't block since this is only called when
        // the result is ready or an exception has occured
        return f(finished.get());
      });
}

// 10.8 Monadic composition
// Monads can be composed together much like how functions can have 
// their outputs go to function's input
// M<std::string> is some monadic container for a string
// We can create a function that composes two functions

// Compose functions F and G
template <typename F, typename G>
auto mcompose(F f, G g)
{
  return [=] (auto value)
  {
    return mbind(f(value), g);
  }
}

// Can combine in this way:
// auto user_html = mcompose(user_full_name, to_html);
//
// auto grandchildren = mcompose(children, children);
//
// Composing a monadic function with the constructor function
// for that monad results in the same function
// 
// mcompose(f, construct) == f
// mcompose(construct, f) == f
//
// Kleisli composition - associativity law says that the order of
// composition doesn't matter
//
// mcompose(f, mcompose(g,h)) == mcompose(mcompose(f, g), h)


int main(int argc, char** argv)
{
  return 0;
}
