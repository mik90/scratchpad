#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <vector>
#include <mutex>
#include <map>
#include <type_traits>

template <typename F>
class lazy_val {
  private:
    F m_computation; // Function that defined computation
    mutable decltype(m_computation()) m_cache; // type is the return type of the computation
    mutable std::once_flag m_value_flag;
  public:
    // This requires that the result type of 'computation'
    // is default constructible
    lazy_val(F computation) : m_computation(computation) {};
    // Casts an instance of lazy_val to the const& of computation's
    // return type
    operator const decltype(m_computation())& () const {
      std::call_once(m_value_flag, [this] {
          m_cache = m_computation();
          });
      return m_cache;
    }
};

// For non C++17 compilers
template <typename F>
inline lazy_val<F> make_lazy_val(F&& computation) {
  // Creates lazy_val instance by deducing the type of the computation
  return lazy_val<F>(std::forward<F>(computation)); }

class person_t {
  public:
    enum gender_t { female, male };
    enum output_format_t { name_only, full_name };
    person_t(std::string name, int age, enum gender_t g, std::string surname) 
        : m_name(name), m_age(age), m_gender(g), m_surname(surname) { };
    std::string name() const {return m_name;}
    std::string surname() const {return m_surname;}
    int age() const {return m_age;}
    enum gender_t gender() const {return m_gender;}
    std::string m_name;
    std::string m_surname;
    int m_age;
    enum gender_t m_gender;
};

class fib_cache {
  private:
    unsigned int m_previous;
    unsigned int m_last;
    unsigned int m_size;
  public:
    fib_cache() : m_previous{0}, m_last{1}, m_size{2} {};
    size_t size() const { return m_size; }
    unsigned int operator[] (unsigned int n) const {
      return n == m_size - 1 ? m_last :
             n == m_size - 2 ? m_previous :
             0;
    }
    void push_back(unsigned int value) {
      m_size++;
      m_previous = m_last;
      m_last = value;
    }
};

unsigned int fib_naive(unsigned int n) {
  return n == 0 ? 0 : n == 1 ? 1 :
    fib_naive(n - 1) + fib_naive(n - 2);
}

fib_cache fb;

unsigned int fib(unsigned int n) {
  if (fb.size() > n) {
    return fb[n];
  }
  else {
    const auto result = fib(n - 1) + fib(n - 2);
    fb.push_back(result);
    return result;
  }
}

template <typename Result, typename... Args>
auto make_memoized(Result (*f)(Args...))
{
  std::map<std::tuple<Args...>, Result> cache;

  return [f, cache] (Args... args) mutable -> Result {
    const auto args_tuple = std::make_tuple(args...);
    const auto cached = cache.find(args_tuple);

    if (cached == cache.end()) {
      auto result = f(args...);
      cache[args_tuple] = result;
      return result;
    }
    else {
      return cached->second;
    }
  };
}

class null_param {};

template <class Sig, class F>
class memoize_helper;

template <class Result, class... Args, class F>
class memoize_helper<Result(Args...), F> {
  private:
    using function_type = F;
    using args_tuple_type = std::tuple<std::decay_t<Args>...>;
    function_type f;
    mutable std::map<args_tuple_type, Result> m_cache;
    mutable std::recursive_mutex m_cache_mutex;
  public:
    template <typename Function>
      memoize_helper(Function&& f, null_param) : f(f) {}
      memoize_helper(const memoize_helper& other) : f(other.f) {}

      template<class... InnerArgs>
      Result operator() (InnerArgs&&... args) const {
        std::unique_lock<std::recursive_mutex> lock{m_cache_mutex};
        const auto args_tuple = std::make_tuple(args...);
        const auto cached = m_cache.find(args_tuple);

        if (cached != m_cache.end()) { 
          return cached->second;
        } else {
          auto&& result = f(*this, std::forward<InnerArgs>(args)...);
          m_cache[args_tuple] = result;
          return result;
        }
      }
};

template <class Sig, class F>
memoize_helper<Sig, std::decay_t<F>>
make_memoized_r(F&& f) {
  return {std::forward<F>(f), null_param()};
}

int main()
{
  double A = 5.0; double B = 2.0;
  double P  = A * B;
  // or
  auto P_f = [A, B] () {return A * B;};
  auto fibmemo = make_memoized_r<unsigned int(unsigned int)>(
          [](auto& fib, unsigned int n) {
            std::cout << "Calculating " << n << ".\n";
            return n == 0 ? 0 : n == 1 ? 1 : fib(n - 1) + fib(n - 2);
          });
  std::cout << "Result:" << fibmemo(10) << std::endl;
  return 0;
}
