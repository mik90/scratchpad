#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <iostream>
#include <memory>

// Functional data structures

// Linked list, only destroying nodes that the
// caller owns
template <typename T>
class list
{
  public:
  private:
    struct node
    {
      T value;
      std::shared_ptr<node> tail;

      // Destroy all nodes
      ~node() 
      {
        auto next_node = std::move(tail);
        while (next_node)
        {
          // Check if we're the sole owner of the node
          // Break if we aren't
          if (!next_node.unique()) {break;}
          // Declare a shared node pointer
          std::shared_ptr<node> tail;
          // Change where our tail is
          std::swap(tail, next_node->tail);
          // clear out the data
          next_node.reset();
          // Our next node is now the tail
          next_node = std::move(tail);
        }
      }
    };

    std::shared_ptr<node> m_head;
};

// Bitmapped vector trie (prefix-tree)
// Dynamically creates new vectors when they run out of space
// Values stored in leaf nodes
// Split input index into chunks to figure out which child to go to


int main(int argc, char** argv)
{

  return 0;
}
