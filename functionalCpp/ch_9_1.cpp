#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <istream>
#include <memory>
#include <cassert>
#include <variant>
#include <iterator>

// Note: can use std::tie for making and then comparing pairs

// Algebraic types: products and sums
#ifdef SUM_TYPE_INHERITANCE

// Fake a socket
using socket_t = std::string;

// Sum types through inheritance:
class state_t {
  protected:
    // Don't create instances of this abstract class
    state_t(int type) : type(type) {};
  public:
    virtual ~state_t() {};
    int type; // Quick way to determine class type
};

class init_t : public state_t {
  public:
    enum { id = 0 };
    init_t() : state_t(id) {};
};

class running_t : public state_t {
  public:
    enum { id = 1 };
    running_t() : state_t(id) {};
    // Destructor should close web socket
    unsigned count() const {return m_count;}
  private:
    unsigned m_count;
    socket_t m_web_page;
};

class finished_t : public state_t {
  public:
    enum { id = 2 };
    finished_t(unsigned count) : state_t(id), m_count(count) {};
    unsigned count() const {return m_count;}
  private:
    unsigned m_count;
};

class program_t {
  public:
    program_t() : m_state(std::make_unique<init_t>()) {}
    void counting_finished()
    {
      assert(m_state->type == running_t::id);
      // We know the current state is running_t
      auto state = static_cast<running_t*>(m_state.get());
      // Switch to finished state, pass in current state's
      // count as an argument. Previous state is auto-destroyed
      m_state = std::make_unique<finished_t>(state->count());
    }
  private:
      std::unique_ptr<state_t> m_state;
};

#elif SUM_TYPE_VARIANT
// Sum types through unions and std::variant:
class init_t { };

class running_t {
  public:
    running_t(const std::string& url) : m_web_page(url) {};
    void count_words()
    {
      m_count = std::distance(std::istream_iterator<std::string>(m_web_page),
                              std::istream_iterator<std::string>());
    }
    unsigned count() const {return m_count;}
  private:
    unsigned m_count = 0;
    std::istream m_web_page;
};

class finished_t {
  public:
    finished_t(unsigned count) : m_count(count) {};
    unsigned count() const {return m_count;}
  private:
    unsigned m_count;
};

class program_t {
  public:
    program_t() : m_state(init_t()) {}
    void counting_finished()
    {
      // Get the state if it's a running_t
      const auto* state = std::get_if<running_t>(&m_state);
      assert(state != nullptr);
      m_state = finished_t(state->count());
    }
  private:
      std::variant<init_t, running_t, finished_t> m_state;
};

#else
// Sum types with std::optional
struct nothing_t {};
template <typename T>
using optional = std::variant<nothing_t, T>;

// std::get_if for std::optional
template <typename T, typename Variant>
std::optional<T> get_if(const Variant& variant)
{
  T* ptr = std::get_if<T>(&variant);
  if (ptr) 
    return *ptr;
  else
    return std::optional<T>();
}

/* counting_finished could be reimplimented and
   we can remove the need to return a pointer
   since it returns a std::optional
void counting_finished()
{
  auto state = get_if<running_t>(m_state);
  assert(state.has_value());
  m_state = finished_t(state->count());
}
*/


#endif

// Sum types for error handling
// Can have option with error code, std::exception_ptr,
// or anything else

// Simple error return, requires manual constructing
// and destructing of T and E. Also should make functions
// for both T and E
template<typename T, typename E>
class expected
{
  private:
    union {
      T m_value;
      E m_error;
    };
    bool m_valid;
    T& get()
    {
      if (!m_valid)
        throw std::logic_error("Missing a value");
      return m_value;
    }
    E& error()
    {
      if (m_valid)
        throw std::logic_error("There is no error");
      return m_error;
    }
    template <typename... Args>
    static expected success(Args&&... params)
    {
      expected result;
      result.m_valid = true;
      // Init the value of type T in the memory location of
      // m_value. Forward over the params of type Args.
      // This uses already-allocated memory and constructs
      // an object inside of it.
      new (&result.m_value) T(std::forward<Args>(params)...);
      return result;
    }
    template <typename... Args>
    static expected error(Args&&... params)
    {
      expected result;
      result.m_valid = false;
      // Do the same for type E
      new (&result.m_value) E(std::forward<Args>(params)...);
      return result;
    }
    ~expected()
    {
      // Call the appropriate destructor
      if (m_valid)
        m_value.~T();
      else
        m_error.~E();
    }
    // Copy constructor
    expected(const expected& other) : m_valid(other.m_valid)
    {
      // Construct 'other' in our union
      if (m_valid)
        new (&m_value) T(other.m_value);
      else
        new (&m_error) E(other.m_error);
    }
    // Move constructor
    expected(expected&& other) : m_valid(other.m_valid)
    {
      // Construct 'other' in our union
      if (m_valid)
        new (&m_value) T(std::move(other.m_value));
      else
        new (&m_error) E(std::move(other.m_error));
    }
    // Implement the assignment operator with the copy-and-swap idiom
    void swap(expected& other)
    {
      using std::swap;
      if (m_valid)
      {
        if (other.m_valid)
        {
          // Both contain valid values, so swap
          swap(m_value, other.m_value);
        }
        else
        {
          // This value is valid, other is an error
          // Store error in temp variable
          auto temp = std::move(other.m_error);
          other.m_error.~E();
          // Move our value into 'other'
          new (&other.m_value) T(std::move(m_value));
          // Then destroy it
          m_value.~T();
          // Move the temp error in our union
          new (&m_error) E(std::move(temp));
          // Swap our valid bool with other's
          std::swap(m_valid, other.m_valid);
        }
      }
      else
      {
        if (other.m_valid)
        {
          // This contains an error, other is valid.
          // Use the previous case as the implementation
          // Where 'this' is valid, other is error
          other.swap(*this);
        }
        else
        {
          // Both are errors, just swap error values
          swap(m_error, other.m_error);
        }
      }
    }
    // Assignment operator
    expected& operator= (expected other)
    {
      // Use our swap method to swap
      swap(other);
      return *this;
    }
    // Casting operator for bool
    operator bool() const
    {
      return m_valid;
    }
    // Casting operator for std::optional
    operator std::optional<T>() const
    {
      if (m_valid)
        return m_value;
      else
        // If the code uses optional, just
        // omit the error in place of nothingness
        return std::optional<T>();
    }
    // Implement get_if. Error type is just std::string
    template <typename R, typename Variant,
             typename Expected = expected<T, std::string>>
    expected get_if(const Variant& variant)
    {
      R* ptr = std::get_if<R>(variant);

      if (ptr)
        return Expected::success(*ptr);
      else
        return Expected::error("Variant doesn't contain the expected type");
    }
};


int main(int argc, char** argv)
{

  return 0;
}
