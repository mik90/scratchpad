#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <variant>
#include "../../Mach7/code/mach7/match.hpp"

// 9.4 Pattern matching with Mach7 library

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
		Match(m_state)
		{
			Case(C<normal_scoring>())
			
			// If player who had 40 points won the ball,
			// they won the game. Don't need to care about
			// other player
			Case(C<forty_scoring>(which_player, _))
			
			// If the ball is won by player who doesn't
			// have 40 points, and the current number of points
			// for that player is 30, the game is in deuce
			Case(C<forty_scoring>(_, 30))
			
			// If neither case match, increase number of points
			// for the player
			Case(C<forty_scoring>())
			
			Case(C<deuce>())
			
			Case(C<advantage>())
		}
		EndMatch
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
