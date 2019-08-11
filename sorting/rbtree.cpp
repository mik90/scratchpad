#include <iostream>
#include <vector>
#include <memory>
#include <type_traits>


/*
 * Red-Black tree properties:
 * 1. Nodes are red or black
 * 2. Root is black
 * 3. All leaves are black and nullptr
 * 4. If a node is red, both its children are black
 * 5. Every path from a given node to any of its descendant
 *    leaves (nullptr nodes) contains the same number of
 *    black nodes
 */
template<typename T>
class RedBlackTree
{
  public:
    enum color_t {RED, BLACK};
    RedBlackTree()
    {
      // Default constructor
    }
    RedBlackTree(const T& collection)
    {
      for (const auto& c : collection)
      {
        
      }
    }
  private:
    struct Node
    {
      T value;
      std::unique_ptr<Node> parent;
      std::unique_ptr<Node> left;
      std::unique_ptr<Node> right;
      enum color_t color;
      Node(T& value) : value(value)
      {
        parent = nullptr;
        left   = nullptr;
        right  = nullptr;
        // New nodes are red
        color  = RED;
      }
    };
    std::unique_ptr<Node> m_root;

    std::unique_ptr<Node>&
      getParent(const std::unique_ptr<Node>& n)
    {
      return n->parent;
    }

    std::unique_ptr<Node>&
      getGrandparent(const std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node> parent = getParent(n);
      // Root node may not have a parent
      if (parent == nullptr)
        return nullptr;

      return getParent(n);
    }

    std::unique_ptr<Node>&
      getSibling(const std::unique_ptr<Node>& n)
    {
      // Go to parent
      std::unique_ptr<Node> parent = getParent(n);
      if (parent == nullptr)
        return nullptr;

      // Don't return ourself
      if (n == parent->left)
        return parent->right;
      else
        return parent->left;
    }

    std::unique_ptr<Node>&
      getUncle(const std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node> parent      = getParent(n);
      std::unique_ptr<Node> grandparent = getGrantParent(n);

      // Can't get a grandparent? Can't get an uncle
      if (grandparent == nullptr)
        return nullptr;

      return getSibling(parent);
    }

    // Rotations ----------
    void rotateLeft(std::unique_ptr<Node> n)
    {
      /*              *  - parent
       *              | 
       *             [*] - n
       *             / \
       *                * - newNode, will be 
       *                    moved up and left
       */
      std::unique_ptr<Node> newNode = n->right;
      std::unique_ptr<Node> parent  = getParent(n);

      // Cannot make leaves into internal nodes
      if (newNode == nullptr)
      {
        std::cerr << "Could not rotate left\n";
        return;
      }

      // Swap newNode with n
      // it will be n's parent and n will
      // take over its children
      n->right      = newNode->left;
      newNode->left = n;
      n->parent     = newNode;

      // Update parent pointers
      if (n->right != nullptr)
        n->right->parent = n;

      // Only update parent's children if n
      // is not root
      if (parent != nullptr)
      {
        // Update parent's children
        // n is no longer their child 
        if (n == parent->left)
          parent->left = newNode;
        else if (n == parent->right)
          parent->right = newNode;
      }

      newNode->parent = parent;
      return;
    }
    
    void rotateRight(std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node> newNode = n->left;
      std::unique_ptr<Node> parent  = getParent(n);
      
      // Cannot make leaves into internal nodes
      if (newNode == nullptr)
      {
        std::cerr << "Could not rotate right\n";
        return;
      }

      // Swap newNode with n
      // it will be n's parent and n will
      // take over its children
      n->left        = newNode->right;
      newNode->right = n;
      n->parent      = newNode;

      // Update parent pointers
      if (n->left != nullptr)
        n->left->parent = n;
      
      // Only update parent's children if n
      // is not root
      if (parent != nullptr)
      {
        // Update parent's children
        // n is no longer their child 
        if (n == parent->right)
          parent->right = newNode;
        else if (n == parent->left)
          parent->left = newNode;
      }

      newNode->parent = parent;
      return;
    }

    void insert(T& value)
    {
      // Take the new value in by reference, the Node constructor
      // will make its own copy of value 
      if (!std::is_copy_constructible<T>::value)
      {
        std::cerr << "Type " << typeid(T).name() << "is not copy constructible\n";
        return;
      }

      std::unique_ptr<Node> n = std::make_unique<Node>(value);

      insertRecursive(m_root, n);

      repairTree(n);

      // Find new root, in case there is one
      m_root = n;
      while (getParent(m_root) != nullptr)
      {
        m_root = getParent(m_root);
      }

      return;
    } 

    void insertRecursive(std::unique_ptr<Node>& root, std::unique_ptr<Node>& n)
    {
      // Descend until a leaf is found
      //
      // The root isn't null and our key is less than the root's
      if (root != nullptr && n->key < root->key)
      {
        if (root->left != nullptr)
        {
          // Recurse down to the left until we hit a leaf 
          insertRecursive(root->left, n);
          return;
        }
        else
        {
          // Left is a leaf so we'll insert n down there
          root->left = n;
        }
      }
      else if(root != nullptr)
      {
        // n->key >= root->key
        if(root->right != nullptr)
        {
          // Recurse down to the right until we hit a leaf 
          insertRecursive(root->right, n);
          return;
        }
        else
        {
          // Right is a leaf so we'll insert n down there
          root->right = n;
        }
      }

      // Update the values of our new node
      n->parent = root;
      n->left   = nullptr;
      n->right  = nullptr;
      n->color  = RED;
      return;
    }

    void repairTree(std::unique_ptr<Node>& n)
    {
      if (getParent(n) == nullptr)
      {
        // n is the root
        n->color = BLACK;
      }
      else if (getParent(n)->color == BLACK)
      {
        /* Do nothing, adding a red node will not violate
         * property 5 or any other properties*/
      }
      else if (getUncle(n) != nullptr && getUncle(n)->color == RED)
      {
        // Parent is RED and the uncle is RED, the problem is that
        // we are also RED. Paint our parent and uncle BLACK.
        getParent(n)->color      = BLACK;
        getUncle(n)->color       = BLACK;
        // Paint our grandparent RED so we don't violate property 4
        getGrandparent(n)->color = RED;
        // Repair the tree so we don't violate property 2
        repairTree(getGrandparent(n));
      }
      else
      {
        // Parent is RED and n is BLACK
        std::unique_ptr<Node> parent      = getParent(n);
        std::unique_ptr<Node> grandparent = getGrandparent(n);

        if (n == parent->right && parent == grandparent->left)
        {
          // n is a right child and p is a left child
          rotateLeft(parent);
          n = n->left;
        }
        else if (n == parent->left && parent == grandparent->right)
        {
          rotateRight(parent);
          n = n->right;
        }

        parent      = getParent(n);
        grandparent = getGrandparent(n);

        // If we're on the left, rotate right.
        // If we're on the right, rotate left.
        if (n == parent->left)
          rotateRight(grandparent);
        else
          rotateLeft(grandparent);

        parent->color      = BLACK;
        grandparent->color = RED;
      }
      return;
    }

    void replaceNode(std::unique_ptr<Node> n, std::unique_ptr<Node> child)
    {
      // Child's parent will be our parent
      child->parent = n->parent;

      // If we're on the left, our parent's left child
      // should be child.
      // If we're on the right, our parent's right child
      // should be child;
      if (n == n->parent->left)
        n->parent->left = child;
      else
        n->parent->right = child;
    }

    void deleteOneChild(std::unique_ptr<Node>& n)
    {
      // Note: n has one null and one non-null child
      //
      // Get the non-null child
      std::unique_ptr<Node> child = (n->right == nullptr) ?
                                        n->left :
                                        n->right;
      if (child == nullptr)
      {
        std::cerr << "Child is null when it shouldn't be\n";
        return;
      }

      replaceNode(n, child);
      if (n->color == BLACK)
      {
        if (child->color == RED)
          child->color = BLACK;
        else
          deleteCase1(child);
      }

      n.reset(); // Clear Node 
      return;
    }

    void deleteCase1(std::unique_ptr<Node>& n)
    {
      if (n->parent != nullptr)
        deleteCase2(n);
      return;
    }
    
    void deleteCase2(std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node>& sibling = getSibling(n);
      if (sibling->color == RED)
      {
        n->parent->color = RED;
        sibling->color   = BLACK;
        if (n == n->parent->left)
          rotateLeft(n->parent);
        else
          rotateRight(n->parent);
      }

      deleteCase3(n);
    }

    void deleteCase3(std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node>& sibling = getSibling(n);
      if ((n->parent->color == BLACK) && (sibling->color == BLACK) &&
          (sibling->left->color == BLACK) && (sibling->right->color == BLACK))
      {
        sibling->color = RED;
        deleteCase1(n->parent);
      }
      else
      {
        deleteCase4(n);
      }
      return;
    }

    void deleteCase4(std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node>& sibling = getSibling(n);
      if ((n->parent->color == RED) && (sibling->color == BLACK) &&
          (sibling->left->color == BLACK) && (sibling->right->color == BLACK))
      {
        sibling->color   = RED;
        n->parent->color = BLACK;
      }
      else
      {
        deleteCase5(n);
      }
      return;
    }

    void deleteCase5(std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node>& sibling = getSibling(n);

      if (sibling->color == BLACK)
      {
        if ((n == n->parent->left) && (sibling->right->color == BLACK) &&
            (sibling->left->color == RED))
        {
          sibling->color       = RED;
          sibling->left->color = BLACK;
          rotateRight(sibling);
        }
        else if((n == n->parent->right) && (sibling->left->color == BLACK) &&
                (sibling->right->color == RED))
        {
          sibling->color       = RED;
          sibling->left->color = BLACK;
          rotateLeft(sibling);
        }
      }
      deleteCase6(n);
      return;
    }


    void deleteCase6(std::unique_ptr<Node>& n)
    {
      std::unique_ptr<Node>& sibling = getSibling(n);

      sibling->color   = n->parent->color;
      n->parent->color = BLACK;

      if (n == n->parent->left)
      {
        sibling->right->color = BLACK;
        rotateLeft(n->parent);
      }
      else
      {
        sibling->left->color = BLACK;
        rotateRight(n->parent);
      }
      return;
    }

};

int main(int arg, char** argv)
{
  std::vector<int> v = {1,5,3,6,4,43,12,6,3,23};
  return 0;
}
