#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <vector>
//#include <boost/phoenix/phoenix.hpp>

//using namespace boost::phoenix::arg_names;


class person_t {
  public:
    enum gender { female, male };
    person_t(std::string name, int age, enum gender g) : m_name(name), m_age(age),
      m_gender(g) { };
    std::string m_name;
    int age() const {return m_age;}
    enum gender gender() const {return m_gender;}
    int m_age;
    enum gender m_gender;
};


class company_t {
  public:
    std::string team_name_for(const person_t& employee) const {
      return "name";
    }
    int count_team_members(const std::string& team_name) const;
  private:
    std::vector<person_t> m_employees;
};

int company_t::count_team_members(const std::string& team_name) const
{
  return std::count_if(m_employees.cbegin(), m_employees.cend(),
      [this, &team_name] (const person_t& employee)
      {
        return team_name_for(employee) == team_name;
      }
    );
}

class error_test_t {
  public:
    error_test_t(bool error = true) : m_error(error) { };

    template <typename T>
      bool operator() (T&& value) const {
        return m_error == (bool) std::forward<T>(value).error();
      }
  private:
    bool m_error;
};

int main()
{
  std::vector<person_t> v = { {"Bob", 23, person_t::male},
       {"Alice", 46, person_t::female}, {"Jen", 69, person_t::female},
       {"John", 55, person_t::male},    {"Fred", 12, person_t::male} };
  std::vector<person_t> females;
  auto predicate = [limit = 42] (auto&& object) {
      return object.age() > limit;
      };
  error_test_t error(true);    
  error_test_t not_error(false);

  //std::vector<int> numbers{21,5,1,3,99,88};
  //std::partition(numbers.begin(), numbers.end(), arg1 <= 42);
  std::string str("hello");
  std::function<bool(std::string)> test_function;
  test_function = &std::string::empty;
  std::cout << test_function(str) << std::endl;

  return 0;
}
