#include <iostream>

template <typename T>
struct Node
{
    T value;
    Node<T>* left;
    Node<T>* right;
    Node(const T& init_value) : value(init_value), left(nullptr), right(nullptr)
    {
      // Construct via const reference
    }
};

template <typename T>
class Tree
{
  public:
    Tree() : m_root(nullptr)
    {
      // Default constructor
    }

    void insert(const T& value)
    {
      // Insert node via value
      if (m_root == nullptr)
      {
        // First node in the tree
        m_root = new Node<T>(value);
      }
      else
      {
        // Search for where it belongs
        insertHelper(m_root, value);
      }
      m_size++;
    }
    unsigned int size() { return m_size; };

  private:
    Node<T>* m_root;
    unsigned int m_size;

    void insertHelper(Node<T>* root, const T& value)
    {
      if (value > root->value)
      {
        // Go to the right
        if (root->right == nullptr)
        {
          // There is a free space there
          root->right = new Node<T>(value);
          return;
        }
        else
        {
          // Keep searching
          insertHelper(root->right, value);
        }
      }
      else
      {
        // Go to the left 
        if (root->left == nullptr)
        {
          // There is a free space there
          root->left = new Node<T>(value);
          return;
        }
        else
        {
          // Keep searching
          insertHelper(root->left, value);
        }
      }
    }
};

int main(int argc, char** argv)
{
  Tree<int> t;
  t.insert(1);
  t.insert(2);
  std::cout << "Size:" << t.size() << "\n";
  return 0;
}
