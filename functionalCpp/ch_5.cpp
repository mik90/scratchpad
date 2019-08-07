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
    // Works on normal values and lvalues
    person_t with_name(const std::string& name) const & {
      person_t result(*this); // Create copy
      result.m_name = name;
      return result;
    }
    // Works on temporaries and other rvalue references
    // Don't add const since that would prevent us from
    // moving the data
    person_t with_name(const std::string& name) && {
      person_t result(std::move(*this)); // Create copy
      result.m_name = name;
      return result;
    }

};

// For constant member functions, the class data must be unchanged
// or all changes must be synchronized over concurrent invocations.
// Changes should seem atomic as far as users of the object are
// concerned 

int main()
{
  std::cout << "Result:" << std::endl;
  return 0;
}
