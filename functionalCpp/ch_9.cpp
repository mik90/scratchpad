#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <memory>
#include <cassert>

// Fake a socket
using socket_t = unsigned;

// Note: can use std::tie for making and then comparing pairs

// Algebraic types: products and sums

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

// Sum types through unions and std::variant:

int main(int argc, char** argv)
{

  return 0;
}
