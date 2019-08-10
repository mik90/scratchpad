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

// 9.3 Handling algebraic data types with pattern matching

// Create function object that can be used on each type
// in a variant instance
//
// Overloaded template takes a list of function objects and
// creates a new function object which exposes the call
// operators of all provided function objects as its own
template <typename... Ts>
struct overloaded : Ts... {
	// Grab the call operator from the function object
	using Ts::operator()...;
};
template <typename... Ts> overloaded(Ts...) -> overloaded<Ts...>;

class tennis_t {
	public:
	enum class points { love, fifteen, thirty };
	enum class player { player_1, player_2 };
	struct normal_scoring { 
		points player_1_points;
		points player_2_points;
	};
	struct forty_scoring {
		player leading_player;
		points other_player_scores;
	};
	struct deuce {};
	struct advantage {
		player player_with_advantage;
	};
	void point_for(player which_player)
	{
		// visit calls the 'overloaded' function object
		std::visit(
			overloaded {
				[&](const normal_scoring& state) {
					// Increment score or switch state
				},
				[&](const forty_scoring& state) {
					// Player wins, or both get to 40 so 
					// switch to deuce
				},
				[&](const deuce& state) {
					// Switch to advantage on score
				},
				[&](const advantage& state) {
					// A player wins or we switch back to deuce
				}
			},
			m_state);
	}

	std::variant<
		normal_scoring,
		forty_scoring,
		deuce,
		advantage> m_state;
};

int main(int argc, char** argv)
{

  return 0;
}
