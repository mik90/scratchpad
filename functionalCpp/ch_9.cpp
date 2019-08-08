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
int main(int argc, char** argv)
{

  return 0;
}
