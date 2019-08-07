#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <vector>

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
    void print(std::ostream& out, output_format_t format) const
    {
      if (format == person_t::name_only) {
        out << m_name << std::endl;
      }
      else if (format == person_t::full_name) {
        out << m_name << " " << m_surname << std::endl;
      }
    }
};
void print_person(const person_t& person, std::ostream& out, person_t::output_format_t format) {
  if (format == person_t::name_only) { out << person.name() << std::endl;}
  else if (format == person_t::full_name) {
    out << person.name() << " " << person.surname() << std::endl;}
}

// Lifting for a single item
template <typename Function>
auto pointer_lift(Function f) {
  return [f] (auto* item) { if (item) f(*item); };
}
// Lifting for a collection
template <typename Function>
auto collection_lift(Function f) {
  return [f] (auto& items) { for (auto& item : items) f(item);  };
}

// Reversing pairs
template <typename C, typename P1 = typename std::remove_cv<typename C::value_type::first_type>::type,
         typename P2 = typename C::value_type::second_type>
std::vector<std::pair<P2,P1>> reverse_pairs(const C& items) {
  std::vector<std::pair<P2,P1>> result();
  std::transform(std::begin(items), std::end(items), std::begin(result),
      [] (const std::pair<const P1, P2>& p) { return std::make_pair(p.second,p.first); });
      return result;
}

// In currying, must bind the arguments in order
auto greater_curried(double first) {
  return [first] (double second) { return first > second;};
}

int main()
{
  std::vector<person_t> v = { {"Bob", 23, person_t::male, "Bobert"},
       {"Alice", 46, person_t::female, "Alicebert"}, {"Jen", 69, person_t::female, "Jenbert"},
       {"John", 55, person_t::male, "Johnbert"},    {"Fred", 12, person_t::male, "Fredbert"} };
  using namespace std::placeholders;
  // Run the same function but have _1 take in a different person due to the iteration
  // bind stores copies of the boudn values in the function object
  //std::for_each(v.cbegin(), v.cend(), 
  //              std::bind(&person_t::print, _1, std::ref(std::cout), person_t::full_name));
  std::cout << "Result:" << greater_curried(1)(5) << std::endl;
  return 0;
}
