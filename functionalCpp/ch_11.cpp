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

// 11.3 Making curried functions
//
// This can store a callable object and any number of
// function arguments. This can be enough to call the 
// callable object or it can not be enough. 

// Due to class members that are functions, it's hard to write code that will
// call all types of functions
// std::invoke was made to deal with this. Used when you don't know the exact
// type of the callable object. std::apply is similar but accepts a tuple of
// arguments instead of individual arguments

template <typename Function, typename... CapturedArgs>
class curried
{
  private:
    using CapturedArgsTuple = std::tuple<std::decay_t<CapturedArgs>...>;
    Function m_function;
    std::tuple<CapturedArgs...> m_captured;

    template <typename... Args>
    static auto capture_by_copy(Args&&... args)
    {
      return std::tuple<std::decay_t<Args>...>
        (std::forward<Args>(args)...);
    }
  public:
    curried(Function f, CapturedArgs... args) : m_function(f),
        m_captured(capture_by_copy(std::move(args)...))
    { }
    curried(Function f, std::tuple<CapturedArgs...> args) : m_function(f),
        m_captured(std::move(args))
    { }

    template<typename... NewArgs>
    auto operator() (NewArgs&&... args) const
    {
      // Create tuple out of arguments
      auto new_args = capture_by_copy(std::forward<NewArgs>(args)...);
      // Concatenate new arguments onto old ones
      auto all_args = std::tuple_cat(m_captured, std::move(new_args));

      // Check if we now have enough arguments to call the function
      if constexpr(std::is_invocable_v<Function, CapturedArgs..., NewArgs...>)
      {
        // Call the function
        return std::apply(m_function, all_args);
      }
      else
      {
        // Return a new instance of curried, with an updated
        // amount of arguments
        return curried<Function, CapturedArgs..., NewArgs...>(
            m_function, all_args);
      }
    }

};

// 11.4 Domain Specific Language (DSL) Building blocks
// Similar to writing a library except in a smaller domain
// and is generalized to a specific codebase and usage case

// Example: generalizing records of transactions
// with(martha) {
//  name = "Martha"
//  surname = "Jones"
//  age = 42
//  };

// Best approach is to draw out an abstract syntax tree

// Create a dummy structure called 'field' that can hold anything

// In order to assign, overriding the assignment
// operator isn't enough. Also  return an AST node 'update'
// that defines an update operation. This can store the
// member pointer along with the value

// Now the call operator needs to be implemented for the
// update node.
// 3 cases:
// - pointer to a member variable that can be changed directly
// - ordinary setter function
// - setter functions that returns a bool that indicates whether
//   or not the update succeeded
// std::invocable_r checks if a function can be invoked as well as
// if it'll return a certain type
template<typename Member, typename Value>
struct update {
  update(Member member, Value value) : member(member), value(value) {}
  Member member;
  Value value;

  template <typename Record>
  bool operator() (Record& record)
  {
    if constexpr (std::is_invocable_r<bool, Member, Record, Value>())
    {
      // Setter function that can fail
      return std::invoke(member, record, value);
    }
    else if constexpr (std::is_invocable<Member, Record, Value>())
    {
      // Result type isn't bool or convertible to bool, so invoke it
      // and just return true
      return std::invoke(member, record, value);
      return true;
    }
    else
    {
      // We have a pointer that points directly to a member variable
      std::invoke(member, record, value) = value;
      return true;
    }
  }
};

// Override the assigment operator

template<typename Member>
struct field {
  field(Member member) : member(member) {}

  template <typename Value>
  update<Member, Value> operator=(const Value& value) const
  {
    return update{member, value};
  }
  Member member;
};

// This deals with the assignment but now the 'with' function
// needs to be created.

// With transactions, we are probably dealing with a database. We care
// whether or not the transaction completed. If the update fails, we
// should cancel the transaction. A copy-and-swap ensures that we can
// copy the original record, try to modify it, and swap it if the updates
// were successful.
template <typename Record>
class transaction
{
  private:
  Record& m_record;
  
  template<typename... Updates>
  bool all(Updates... results) const
  {
    // Collect all the results of different updates,
    // returns true if they all succeeded
    return (... && results);
  }

  public:
    transaction(Record& record) : m_record(record) {}
    template <typename... Updates>
    bool operator() (Updates... updates)
    {
      auto temp = m_record;
      // Apply all of the updates
      if (all(updates(temp)...))
      {
        // If successful, swap the temp with the original
        std::swap(m_record, temp);
        return true;
      }
      else
      {
        return false;
      }
    }

};

template <typename Record>
auto with(Record& record)
{
  return transaction(record);
}

int main(int argc, char** argv)
{
  return 0;
}
