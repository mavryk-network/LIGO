#include <stdlib.h>
#include <stdbool.h> 
#include "api.h"

typedef struct Node {
  bool isBlack;
  struct Node *parent;
  int *value;
  struct Node *leftChild;
  struct Node *rightChild;
} Node;

Node *newNode(int *value, Node *parent) {
  Node *newNode_ = (Node *)(malloc(sizeof(Node)));
  newNode_->isBlack    = false;
  newNode_->parent     = NULL;
  newNode_->value      = value;
  newNode_->leftChild  = NULL;
  newNode_->rightChild = NULL;
  return newNode_;
}

Node *cloneNode(Node *node) {
  Node *clonedNode = (Node *)(malloc(sizeof(Node)));
  clonedNode->isBlack    = node->isBlack;
  clonedNode->parent     = node->parent;
  clonedNode->value      = node->value;
  clonedNode->leftChild  = node->leftChild;
  clonedNode->rightChild = node->rightChild;
  return clonedNode;
}

typedef int8_t (*__compare)(int * item_a, int * item_b);
void insertCase1 (Node *node);

Node *rotateLeft(Node *node) {
  Node *newTop = node->rightChild;
  Node *newRightChild = node->leftChild;
  newTop->leftChild = node;
  newTop->rightChild = newRightChild;
  node->parent = newTop;
  if (newRightChild != NULL) {
    newRightChild->parent = node;
  }
  return newTop;
}

Node *rotateRight(Node *node) {
  Node *newTop = node->leftChild;
  Node *newLeftChild = node->rightChild;
  newTop->rightChild = node;
  newTop->leftChild = newLeftChild;
  node->parent = newTop;
  if (newLeftChild != NULL) {
    newLeftChild->parent = node;
  }
  return newTop;
}

Node *uncle(Node *node) {
  if (node == node->parent->leftChild) {
    return node->parent->rightChild;
  } else {
    return node->parent->leftChild;
  }
}

void insertCase5(Node *node) {
  node->parent->isBlack = true;
  node->parent->parent->isBlack = false;
  if (node == node->parent->leftChild && node->parent == node->parent->parent->leftChild) {
    rotateRight(node);
  } else {
    rotateLeft(node);
  }
}

void insertCase4(Node *node) {
  if (node == node->parent->rightChild && node->parent == node->parent->parent->leftChild) {
    rotateLeft(node->parent);
    node = node->leftChild;
  } else if (node == node->parent->leftChild && node->parent == node->parent->parent->rightChild) {
    rotateRight(node->parent);
    node = node->rightChild;
  }
  insertCase5(node);
}

void insertCase3(Node *node) {
  Node *unc = uncle(node);
  if (unc->isBlack == false) {
    node->parent->isBlack = true;
    unc->isBlack = true;
    node->parent->parent->isBlack = false;
    insertCase1(node->parent->parent);
  } else {
    insertCase4(node);
  }
}

void insertCase2(Node *node) {
  if (node->parent->isBlack == true) {
    return;
  } else {
    insertCase3(node);
  }
}

void insertCase1(Node *node) {
  if (node->parent == NULL) {
    node->isBlack = true;
  } else {
    insertCase2(node);
  }
}

void insertNode(int *value, Node *root, __compare compare) {
  Node *newNode_ = newNode(value, root);

  // insert the node
  Node *node = cloneNode(root);
  while (node) {
    int8_t comp_result = compare(node->value, value);
    if (comp_result == -1) {
      if (node->leftChild != NULL) {
        // clone the node due to FP
        Node *cloned = cloneNode(node->leftChild);
        node->leftChild = cloned; 
        node = cloned;
      } else {
        // insert as left child
        node->leftChild = newNode_;
        break;
      }
    } else if (comp_result == 1) {
      if (node->rightChild != NULL) {
        // clone the node due to FP
        Node *cloned = cloneNode(node->rightChild);
        node->rightChild = cloned; 
        node = cloned;
      } else {
        // insert as right child
        node->rightChild = newNode_;
        break;
      }
    } else {
      return; // is the same, so do nothing
    }
  }

  insertCase1(node);

  // find the right position for insertion
  // use comparison function
  // recreate every node along the way...
}

