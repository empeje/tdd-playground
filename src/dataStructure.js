/** Class representing a Stack. */
class Stack {

  constructor() {
    this._storage = {};
    this._size = 0;
  }
  /*
  * Adds a new value at the end of the stack
  * @param {*} value the value to push
  */
  push(value) {
    this._size ++;
    this._storage[this._size] = value;
  }

  /*
  * Removes the value at the end of the stack and returns it
  * @return {*} the last and newest value in the stack
  */
  pop() {
    if(this._size){
      const results = this._storage[this._size];
      delete this._storage[this._size];
      this._size --;
      return results;
    }
  }
  /*
  * Returns the value at the end of the stack without removing it
  * @return {*} the last and newest value in the stack
  */
  peek() {
    if(this._size){
      return this._storage[this._size];
    }
  }

  size() {
    return this._size;
  }

  empty() {
    this._storage = {};
    this._size = 0;
  }

  swap() {
    const temp = this._storage[this._size];
    this._storage[this._size] = this._storage[this._size - 1];
    this._storage[this._size - 1] = temp;
  }
}

/** Class representing a Queue.
 * @constructor
 */
class Queue {

  constructor() {
    this._storage = {};
    this._head = 0;
    this._size = 0;
  }
  /*
  * Enqueues a new value at the end of the queue
  * @param {*} value the value to enqueue
  */
  enqueue(value) {
    this._storage[this._size + this._head] = value;
    this._size++;
  }

  /*
  * Dequeues the value from the beginning of the queue and returns it
  * @return {*} the first and oldest value in the queue
  */
  dequeue() {
    if(this._size) {
      const results = this._storage[this._head];
      delete this._storage[this._head];
      this._size--;
      this._head++;
      return results;
    }
  }
  /*
  * Returns the value at the beginning of the queue without removing it from the queue
  * @return {*} the first and oldest value in the queue
  */
  peek() {
    if(this._size) {
      return this._storage[this._head];
    }
  }

  size() {
    return this._size;
  }
}

class Node {
  constructor(value) {
    this.value = value;
    this.next = null;
  }
}

/** Class representing a Linked List */
class LinkedList {

  constructor(initialValue) {
    this.head = null;
    this.tail = null;

    if(initialValue) this.insert(initialValue);
  }

  /*
  * Inserts a new value to the end of the linked list
  * @param {*} value - the value to insert
  */
  insert(value) {
    if(this.head) {
      this.tail.next = new Node(value);
      this.tail = this.tail.next;
    } else {
      this.head = new Node(value);
      this.tail = this.head;
    }
  }

  /*
  * Deletes a node
  * @param {*} node - the node to remove
  * @return {*} value - the deleted node's value
  */
  remove(node) {
    let beforeCurrentNode = null;
    let currentNode = this.head;

    while(currentNode.next && currentNode !== node) { // handling general case
      beforeCurrentNode = currentNode;
      currentNode = currentNode.next;
    }

    if(!beforeCurrentNode) {
      this.head = currentNode.next;
    } else if(!currentNode.next) {
      this.tail = beforeCurrentNode;
      this.tail.next = null;
    } else if(beforeCurrentNode && currentNode) {
      beforeCurrentNode.next = currentNode.next;
    }

  }

  /*
  * Removes the value at the end of the linked list
  * @return {*} - the removed value
  */
  removeTail() {
    let currentNode = this.head;
    while(currentNode.next !== this.tail) {
      currentNode = currentNode.next;
    }

    this.tail = currentNode;
    this.tail.next = null;
  }

  search(value) {
    let results = [];
    let currentNode = this.head;
    while(currentNode.next) { // handling general case
      if (currentNode.value === value) results.push(currentNode);
      currentNode = currentNode.next;
    }

    if(currentNode.value === value) results.push(currentNode); //handling begin-end of list

    return results;
  }

  /*
  * Searches the linked list and returns true if it contains the value passed
  * @param {*} value - the value to search for
  * @return {boolean} - true if value is found, otherwise false
  */
  contains(value) {
    let currentNode = this.head;
    while(currentNode.next && currentNode.value !== value) {
      currentNode = currentNode.next;
    }

    return currentNode.value === value;
  }

  /*
  * Checks if a node is the head of the linked list
  * @param {{prev:Object|null, next:Object|null}} node - the node to check
  * @return {boolean} - true if node is the head, otherwise false
  */
  isHead(node) {
    return node === this.head;
  }

  /*
  * Checks if a node is the tail of the linked list
  * @param {{prev:Object|null, next:Object|null}} node - the node to check
  * @return {boolean} - true if node is the tail, otherwise false
  */
  isTail(node) {
    return node === this.tail;
  }
}

/** Class representing a Hash Table */
class HashTable {
  constructor(size) {
    this._size = size;
    this._storage = [];
    this._utilization = 0;
  }
  /*
  * Inserts a new key-value pair
  * @param {string} key - the key associated with the value
  * @param {*} value - the value to insert
  */
  insert(key, value, storage = this._storage) {
    if(this._utilization >= this._size / 2) this.resize(this._size * 2);

    const hash = this._hash(key);
    if(!storage[hash]) storage[hash] = [];
    if(storage[hash].find(value => value[0] === key)) return;
    storage[hash].push([key, value]);
    this._utilization++;
  }

  /*
  * Deletes a key-value pair
  * @param {string} key - the key associated with the value
  * @return {*} value - the deleted value
  */
  remove(key) {
    const hash = this._hash(key);

    const resultIndex = this._storage[hash].findIndex(value => value[0] === key);
    const result = this._storage[hash][resultIndex][1];

    this._storage[hash].splice(resultIndex, 1);

    return result;
  }

  /*
  * Returns the value associated with a key
  * @param {string} key - the key to search for
  * @return {*} - the value associated with the key
  */
  retrieve(key) {
    const hash = this._hash(key);
    const rawQueryResult = this._storage[hash].find(value => value[0] === key);
    if(rawQueryResult) return rawQueryResult[1];
  }

  resize(newSize) {
    const newStorage = [];
    this._size = newSize;
    this._storage.forEach(subStorage => {
      subStorage.forEach(item => {
        this.insert(item[0], item[1], newStorage);
      })
    });

    this._storage = newStorage;
  };

  /*
  * Hashes string value into an integer that can be mapped to an array index
  * @param {string} str - the string to be hashed
  * @param {number} n - the size of the storage array
  * @return {number} - an integer between 0 and n
  */
  _hash(str, size=this._size) {
    let sum = 0;
    for (let i = 0; i < str.length; i++) {
      sum += str.charCodeAt(i) * 3;
    }

    return sum % size;
  }
}

class Tree {
  constructor(value) {
    this.value = value;
    this.children = [];
  }

  insertChild(value) {
    const newChild = new Tree(value);
    this.children.push(newChild);
    return newChild;
  }

  removeChild(childTree) {
    const childIndex = this.children.findIndex(child => child === childTree);
    this.children.splice(childIndex, 1);
  };

  contains(searchValue) {
    if (this.value === searchValue) return true;
    return this.children.reduce((accumulator, child) => {
      return accumulator || child.contains(searchValue);
    }, false);
  }

  find(searchValue) {
    return Tree.find(this, searchValue);
  }

  traverse(func = console.log) {
    Tree.traverse(this, func);
  }

  size() {
    return Tree.size(this);
  }

  static traverse(tree, func) {
    func(tree.value);
    tree.children.forEach(child => {
      this.traverse(child, func);
    })
  }

  static size(tree) {
    const calculateChildSize = (tree) => {
      const childrenSize = tree.children.reduce((accumulator, currentChild) => (calculateChildSize(currentChild) + accumulator), 0);
      return tree.children.length + childrenSize;
    };

    return 1 + calculateChildSize(tree);
  }

  static find(tree, value) {
    if (tree.value === value) return [tree];
    return tree.children.reduce((accumulator, child) => {
      return accumulator.concat(this.find(child, value));
    }, []);
  }
}

class BinaryTree {
  constructor(value) {
    this.value = value;
    this.left = null;
    this.right = null;
  }

  insertChild(value) {
    const newChild = new BinaryTree(value);

    if(!this.left) {
      this.left = newChild;
    } else if(!this.right) {
      this.right = newChild;
    }

    return newChild;
  }

  inOrderTraversal(func = console.log) {
    BinaryTree.inOrderTraversal(this, func);
  }

  preOrderTraversal(func = console.log) {
    BinaryTree.preOrderTraversal(this, func);
  };

  postOrderTraversal(func = console.log) {
    BinaryTree.postOrderTraversal(this, func);
  }

  contains(value) {
    return BinaryTree.contains(this, value);
  }

  countLeaves() {
    return BinaryTree.countLeaves(this);
  };

  // left, root, right
  static inOrderTraversal(tree, func) {
    if (!tree) return;
    this.inOrderTraversal(tree.left, func);
    func(tree.value);
    this.inOrderTraversal(tree.right, func);
  }

  // root, left, right
  static preOrderTraversal(tree, func = console.log) {
    if (!tree) return;
    func(tree.value);
    this.preOrderTraversal(tree.left, func);
    this.preOrderTraversal(tree.right, func);
  }

  // left, right, root
  static postOrderTraversal(tree, func = console.log) {
    if (!tree) return;
    this.postOrderTraversal(tree.left, func);
    this.postOrderTraversal(tree.right, func);
    func(tree.value);
  }

  static contains(tree, value) {
    if (!tree) return false;
    return tree.value === value || this.contains(tree.left, value) || this.contains(tree.right, value);
  }

  static countLeaves(tree) {
    if (!tree) return 0;
    if (!tree.left && !tree.right) return 1;
    return this.countLeaves(tree.left) + this.countLeaves(tree.right);
  }
}

/**
 * Adjacency matrix graph implementation
 */
class AdjacencyMatrixGraph {
  constructor() {
    this.nodes = [];
    this.adjList = {};
  }

  addNode(node) {
  }

  addEdge(node1, node2) {
  }

  removeNode(node) {
  }

  removeEdge(node1, node2) {
  }

  depthFirstTraversal(startingNode, func = console.log) {
  }

  breadthFirstTraversal(startingNode, func = console.log) {
  }
}

/**
 * Adjacency list graph implementation
 */
class Graph {
  constructor() {
    this.adjList = {};
    this.size = 0;
    this.head = 0;
  }

  addNode(node) { // O(1)
    this.adjList[node.value] = {
      node: node,
      edges: []
    };
  }

  addEdge(node1, node2) { // O(1)
    this.adjList[node1.value].edges.push(node2);
    this.adjList[node2.value].edges.push(node1);
  }

  removeNode(node) { // O(n^2)
    const nodeRef = this.adjList[node.value];
    delete this.adjList[node.value];

    nodeRef.edges.forEach(edgeNode => {
      const index = this.adjList[edgeNode.value].edges.indexOf(node);
      this.adjList[edgeNode.value].edges.splice(index, 1);
    });
  }

  removeEdge(node1, node2) { // O(n)
    const node1EdgeIndex = this.adjList[node1.value].edges.indexOf(node1);
    const node2EdgeIndex = this.adjList[node2.value].edges.indexOf(node2);

    this.adjList[node1.value].edges.splice(node1EdgeIndex, 1);
    this.adjList[node2.value].edges.splice(node2EdgeIndex, 1);
  }

  depthFirstTraversal(startingNode, func = console.log) {
  }

  breadthFirstTraversal(startingNode, func = console.log) {
  }
}

export {
  Stack,
  Queue,
  LinkedList,
  HashTable,
  Tree,
  BinaryTree,
  AdjacencyMatrixGraph,
  Graph
}