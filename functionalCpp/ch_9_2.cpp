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

// 9.2 Domain modeling with algebraic data types
// Key point is to have data types that do not allow
// the program to enter invalid states

// Tennis kata, avoid invalid states in a tennis game
// Four states, normal numeric state, state where both players
// are at 40, duece state, and advantage state
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
