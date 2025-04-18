import chai, { expect } from 'chai';
import { describe, beforeEach } from 'mocha'
import sinon from 'sinon';
import sinonChai from 'sinon-chai';
import {
  Stack,
  Queue,
  LinkedList,
  HashTable,
  Tree,
  BinaryTree,
  AdjacencyMatrixGraph,
  Graph,
  BinarySearchTree
} from "../src/dataStructure";

chai.use(sinonChai);

describe('data structure implementation', () => {
  describe('stack', () => {
    let newStack;

    beforeEach(() => {
      newStack = new Stack();
    });

    it('should be able to push value', () => {
      newStack.push('test value');
      expect(newStack._storage[newStack._size]).to.equal('test value');
    });

    it('should be able to push more than one value', () => {
      newStack.push('test value 1');
      expect(newStack._storage[newStack._size]).to.equal('test value 1');
      newStack.push('test value 2');
      expect(newStack._storage[newStack._size]).to.equal('test value 2');
    });

    it('should be able to pop value', () => {
      newStack.push('test value 1');
      expect(newStack._storage[newStack._size]).to.equal('test value 1');
      newStack.push('test value 2');
      expect(newStack._storage[newStack._size]).to.equal('test value 2');

      expect(newStack.pop()).to.equal('test value 2');
      expect(newStack._storage[newStack._size]).to.equal('test value 1');
    });

    it('should be able to peek value', () => {
      newStack.push('test value 1');
      expect(newStack.peek()).to.equal('test value 1');
      newStack.push('test value 2');
      expect(newStack.peek()).to.equal('test value 2');
    });

    it('should be able to see stack size', () => {
      newStack.push('test value 1');
      newStack.push('test value 2');
      expect(newStack.size()).to.equal(2);
    });

    it('should be able to empty stack', () => {
      newStack.push('test value 1');
      newStack.push('test value 2');
      expect(newStack.size()).to.equal(2);
      newStack.empty();
      expect(newStack.size()).to.equal(0);
    });

    it('should not be able to pop empty stack', () => {
      expect(newStack.pop()).to.be.undefined;
    });

    it('should be able to swap two top most elements', () => {
      newStack.push('test value 1');
      newStack.push('test value 2');
      expect(newStack.peek()).to.equal('test value 2');

      newStack.swap();
      expect(newStack.peek()).to.equal('test value 1');
      expect(newStack._storage[newStack.size()]).to.equal('test value 1');
      expect(newStack._storage[newStack.size() - 1]).to.equal('test value 2');
    });
  });

  describe('queue', () => {
    let newQueue;

    beforeEach(() => {
      newQueue = new Queue();
    });

    it('should be able to enqueue', () => {
      newQueue.enqueue('this is test');
      expect(newQueue._storage[newQueue._head]).to.equal('this is test');
    });

    it('should be able to enqueue more than one', () => {
      newQueue.enqueue('this is test 1');
      expect(newQueue._storage[newQueue._head]).to.equal('this is test 1');

      newQueue.enqueue('this is test 2');
      expect(newQueue._storage[newQueue._head]).to.equal('this is test 1');
      expect(newQueue._storage[newQueue._head + 1]).to.equal('this is test 2');
    });

    it('should be able to dequeue', () => {
      newQueue.enqueue('this is test 1');
      newQueue.enqueue('this is test 2');
      newQueue.dequeue();
      expect(newQueue._storage[newQueue._head]).to.equal('this is test 2');
    });

    it('should be able to dequeue more than one', () => {
      newQueue.enqueue('this is test 1');
      newQueue.enqueue('this is test 2');
      newQueue.enqueue('this is test 3');

      expect(newQueue.dequeue()).to.equal('this is test 1');
      expect(newQueue._storage[newQueue._head]).to.equal('this is test 2');

      expect(newQueue.dequeue()).to.equal('this is test 2');
      expect(newQueue._storage[newQueue._head]).to.equal('this is test 3');
    });



    it('should not be able to dequeue empty queue', () => {
      expect(newQueue.dequeue()).to.be.undefined;
    });

    it('should be able to peek', () => {
      newQueue.enqueue('this is test 1');
      expect(newQueue.peek()).to.equal('this is test 1');
    });

    it('should be able to return size', () => {
      newQueue.enqueue('this is test 1');
      expect(newQueue.size()).to.equal(1);

      newQueue.enqueue('this is test 2');
      expect(newQueue.size()).to.equal(2);
    });

    it('should be able to enqueue after a dequeue', () => {
      newQueue.enqueue('this is test 1');
      newQueue.enqueue('this is test 2');
      newQueue.dequeue();
      newQueue.enqueue('this is test 3');
      expect(newQueue._storage[1]).to.equal('this is test 2');
      expect(newQueue._storage[2]).to.equal('this is test 3');
    });
  });

  describe('linked list', () => {
    let newLinkedList;

    beforeEach(() => {
      newLinkedList = new LinkedList();
    });

    it('should be able to insert to list', () => {
      newLinkedList.insert('this is test');
      expect(newLinkedList.head.value).to.equal('this is test');
      expect(newLinkedList.tail.value).to.equal('this is test');
    });

    it('should be able to insert to list more than one', () => {
      newLinkedList.insert('this is test 1');
      expect(newLinkedList.head.value).to.equal('this is test 1');
      expect(newLinkedList.tail.value).to.equal('this is test 1');

      newLinkedList.insert('this is test 2');
      expect(newLinkedList.head.value).to.equal('this is test 1');
      expect(newLinkedList.head.next).to.deep.equal(newLinkedList.tail);
      expect(newLinkedList.tail.value).to.equal('this is test 2');

      newLinkedList.insert('this is test 3');
      expect(newLinkedList.head.value).to.equal('this is test 1');
      expect(newLinkedList.tail.value).to.equal('this is test 3');
    });

    it('should be able to initiate the list with initial value', () => {
      const anotherNewLinkedList = new LinkedList('this is test');
      expect(anotherNewLinkedList.head.value).to.equal('this is test');
    });

    it('should be able to remove tail', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      newLinkedList.insert('this is test 3');

      newLinkedList.removeTail();
      expect(newLinkedList.tail.value).to.equal('this is test 2');
      expect(newLinkedList.tail.next).to.be.null;
    });

    it('should be able to check if linked list contain value', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      expect(newLinkedList.contains('this is test 1')).to.be.true;
      expect(newLinkedList.contains('this is test 2')).to.be.true;
    });

    it('should be able to check if linked list not contain value', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      expect(newLinkedList.contains('this is test 3')).to.be.false;
    });

    it('should be able to check if node is head', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      expect(newLinkedList.isHead(newLinkedList.head)).to.be.true;
      expect(newLinkedList.isHead(newLinkedList.tail)).to.be.false;
    });

    it('should be able to check if node is tail', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      expect(newLinkedList.isTail(newLinkedList.tail)).to.be.true;
      expect(newLinkedList.isTail(newLinkedList.head)).to.be.false;
    });

    it('should be able to search value', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      const nodes = newLinkedList.search('this is test 1');
      expect(nodes[0].value).to.equal('this is test 1');
      expect(nodes.length).to.equal(1);
    });

    it('should return empty array if search value does not exist', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      expect(newLinkedList.search('this is test 3')).to.deep.equal([]);
    });

    it('should be able to remove head', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      newLinkedList.insert('this is test 3');

      newLinkedList.remove(newLinkedList.head);
      expect(newLinkedList.head.value).to.equal('this is test 2');
      expect(newLinkedList.head.next.value).to.equal('this is test 3');
    });

    it('should be able to remove tail', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      newLinkedList.insert('this is test 3');

      newLinkedList.remove(newLinkedList.tail);
      expect(newLinkedList.tail.value).to.equal('this is test 2');
      expect(newLinkedList.tail.next).to.be.null;
    });

    it('should be able to remove node in the middle', () => {
      newLinkedList.insert('this is test 1');
      newLinkedList.insert('this is test 2');
      newLinkedList.insert('this is test 3');
      const [node] = newLinkedList.search('this is test 2');

      newLinkedList.remove(node);
      expect(newLinkedList.head.value).to.equal('this is test 1');
      expect(newLinkedList.head.next).to.deep.equal(newLinkedList.tail);
      expect(newLinkedList.tail.value).to.equal('this is test 3');
    });
  });

  describe('hash table', () => {
    let newHashTable;
    const testData = [
      { key: 'testKey1', value: 'testValue1' },
      { key: 'testKey2', value: 'testValue2' },
      { key: 'testKeyP', value: 'testValue3' },
      { key: 'testKey((', value: 'testValue4' },
    ];

    beforeEach(() => {
      newHashTable = new HashTable(25);
    });

    it('should be able to insert', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[0].value)).to.deep.equal([[testData[0].key, testData[0].value]]);
    });

    it('should be able to handle collision when insert more than one', () => {
      newHashTable.insert(testData[2].key, testData[2].value);
      newHashTable.insert(testData[3].key, testData[3].value);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[2].value)).to.deep.equal([[testData[2].key, testData[2].value], [testData[3].key, testData[3].value]]);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[1][1] === testData[3].value)).to.deep.equal([[testData[2].key, testData[2].value], [testData[3].key, testData[3].value]]);
    });

    it('should be able to insert more than one', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.insert(testData[1].key, testData[1].value);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[0].value)).to.deep.equal([[testData[0].key, testData[0].value]]);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[1].value)).to.deep.equal([[testData[1].key, testData[1].value]]);
    });

    it('should not insert twice', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.insert(testData[1].key, testData[1].value);
      newHashTable.insert(testData[2].key, testData[2].value);
      const itemUnderTest = newHashTable._storage
        .filter(value => value !== undefined)
        .filter(value => value[0][1] === testData[0].value);
      expect(itemUnderTest[0].length).to.equal(1);
    });

    it('should be able to resize', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.insert(testData[1].key, testData[1].value);
      newHashTable.resize(75);

      expect(newHashTable._storage.length).to.greaterThan(25);
      expect(newHashTable._size).to.equal(75);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[0].value)).to.deep.equal([[testData[0].key, testData[0].value]]);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[1].value)).to.deep.equal([[testData[1].key, testData[1].value]]);
    });

    it('should auto resize', () => {
      let counter = 0;
      const insertSomething = () => {
        newHashTable.insert(JSON.stringify(counter), counter);
        counter++;
      };

      for(let i = 0; i <= 14; i++) {
        insertSomething();
      }

      expect(newHashTable._size).to.greaterThan(25);
    });

    it('should be able to remove', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.remove(testData[0].key);
      expect(newHashTable.retrieve(testData[0].key)).to.be.undefined;
    });

    it('should be able to retrieve', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      expect(newHashTable.retrieve(testData[0].key)).to.equal(testData[0].value);
    });
  });

  describe('tree', () => {
    let newTree;

    beforeEach(() => {
      newTree = new Tree('Parent');
    });

    it('should be able to insert child', () => {
      const child = newTree.insertChild('Child 1');

      expect(child.value).to.equal('Child 1');
      expect(child.children).to.deep.equal([]);

      expect(newTree.children[0].value).to.equal('Child 1');
      expect(newTree.children[0].children).to.deep.equal([]);
    });

    it('should be able to insert child of child', () => {
      const child = newTree.insertChild('Child 1');
      const grandchild = child.insertChild('Grandchild 1');

      expect(grandchild.value).to.equal('Grandchild 1');
      expect(grandchild.children).to.deep.equal([]);

      expect(child.children[0].value).to.equal('Grandchild 1');
      expect(child.children[0].children).to.deep.equal([]);
    });

    it('should be able to remove child', () => {
      const child = newTree.insertChild('Child 1');
      newTree.removeChild(child);
      expect(newTree.children[0]).to.be.undefined;
    });

    describe('traverse tree', () => {
      let consoleLogStub;

      beforeEach(() => {
        newTree = new Tree('Parent');
        consoleLogStub = sinon.stub(console, 'log');
      });

      afterEach(() => {
        consoleLogStub.restore();
      });

      it('should be able to traverse tree', () => {
        const child1 = newTree.insertChild('Child 1');
        const grandchild1 = child1.insertChild('Grandchild 1');
        grandchild1.insertChild('Great Grandchild 1');

        const child2 = newTree.insertChild('Child 2');
        const grandchild2 = child2.insertChild('Grandchild 2');
        grandchild2.insertChild('Great Grandchild 2');

        newTree.traverse();
        expect(consoleLogStub).callCount(7);
      });
    });

    it('should be able to get size', () => {
      const child1 = newTree.insertChild('Child 1');
      const grandchild1 = child1.insertChild('Grandchild 1');
      grandchild1.insertChild('Great Grandchild 1');
      const child2 = newTree.insertChild('Child 2');
      const grandchild2 = child2.insertChild('Grandchild 2');
      grandchild2.insertChild('Great Grandchild 2');

      expect(newTree.size()).to.equal(7);
    });

    it('should be able to search and return if tree containing a value or not', () => {
      const child1 = newTree.insertChild('Child 1');
      const grandchild1 = child1.insertChild('Grandchild 1');
      grandchild1.insertChild('Great Grandchild 1');
      const child2 = newTree.insertChild('Child 2');
      const grandchild2 = child2.insertChild('Grandchild 2');
      grandchild2.insertChild('Great Grandchild 2');

      expect(newTree.contains('Great Grandchild 2')).to.be.true;
      expect(newTree.contains('Great Grandchild 5')).to.be.false;
    });

    it('should be able to search and return all result when found', () => {
      const child1 = newTree.insertChild('Child 1');
      const grandchild1 = child1.insertChild('Grandchild 1');
      grandchild1.insertChild('Great Grandchild 1');
      const child2 = newTree.insertChild('Child 2');
      const grandchild2 = child2.insertChild('Grandchild 2');
      const greatGrandchild2 = grandchild2.insertChild('Great Grandchild 2');

      expect(newTree.find('Great Grandchild 2')).to.deep.equal([greatGrandchild2]);
      expect(newTree.find('Great Grandchild 5')).to.deep.equal([]);
    });
  });

  describe('binary tree', () => {
    let newBinaryTree;

    beforeEach(() => {
      newBinaryTree = new BinaryTree('Parent');
    });

    it('should be able to insert node', () => {
      newBinaryTree.insertChild('Child Left');
      newBinaryTree.insertChild('Child Right');

      expect(newBinaryTree.left.value).to.equal('Child Left');
      expect(newBinaryTree.right.value).to.equal('Child Right');
    });

    describe('tree traversing', () =>{
      let consoleLogStub;

      beforeEach(() => {
        consoleLogStub = sinon.stub(console, 'log');
      });

      afterEach(() => {
        consoleLogStub.restore();
      });

      it('should be able to traverse tree in order', () => {
        const newChild1a = newBinaryTree.insertChild('Child 1a');
        const newChild1b = newBinaryTree.insertChild('Child 1b');
        const newChild2a = newChild1a.insertChild('Child 2a');
        const newChild2b = newChild1b.insertChild('Child 2b');
        newChild2a.insertChild('Child 3a');
        newChild2b.insertChild('Child 3b');

        newBinaryTree.inOrderTraversal();
        expect(consoleLogStub).callCount(7);
      });

      it('should be able to traverse tree in pre order', () => {
        const newChild1a = newBinaryTree.insertChild('Child 1a');
        const newChild1b = newBinaryTree.insertChild('Child 1b');
        const newChild2a = newChild1a.insertChild('Child 2a');
        const newChild2b = newChild1b.insertChild('Child 2b');
        newChild2a.insertChild('Child 3a');
        newChild2b.insertChild('Child 3b');

        newBinaryTree.preOrderTraversal();
        expect(consoleLogStub).callCount(7);
      });

      it('should be able to traverse tree in post order', () => {
        const newChild1a = newBinaryTree.insertChild('Child 1a');
        const newChild1b = newBinaryTree.insertChild('Child 1b');
        const newChild2a = newChild1a.insertChild('Child 2a');
        const newChild2b = newChild1b.insertChild('Child 2b');
        newChild2a.insertChild('Child 3a');
        newChild2b.insertChild('Child 3b');

        newBinaryTree.postOrderTraversal();
        expect(consoleLogStub).callCount(7);
      });
    });

    it('should be able to check if tree contains a value', () => {
      const newChild1a = newBinaryTree.insertChild('Child 1a');
      const newChild1b = newBinaryTree.insertChild('Child 1b');
      const newChild2a = newChild1a.insertChild('Child 2a');
      const newChild2b = newChild1b.insertChild('Child 2b');
      newChild2a.insertChild('Child 3a');
      newChild2b.insertChild('Child 3b');

      expect(newBinaryTree.contains('Parent')).to.be.true;
      expect(newBinaryTree.contains('Child 3b')).to.be.true;
      expect(newBinaryTree.contains('Child 4b')).to.be.false;
    });

    it('should be able to count leaves in a tree', () => {
      const newChild1a = newBinaryTree.insertChild('Child 1a');
      const newChild1b = newBinaryTree.insertChild('Child 1b');
      const newChild2a = newChild1a.insertChild('Child 2a');
      const newChild2b = newChild1b.insertChild('Child 2b');
      newChild2a.insertChild('Child 3a');
      newChild2b.insertChild('Child 3b');

      expect(newBinaryTree.countLeaves()).to.equal(2);
    });
  });

  describe('adjacency matrix graph', () => {
    let newAdjacencyMatrixGraph;

    beforeEach(() => {
      newAdjacencyMatrixGraph = new AdjacencyMatrixGraph();
    });

    it('should be able to insert vertices', () => {});

    it('should be able to insert vertices more than one', () => {});

    it('should be able to add edges', () => {});

    it('should be able to delete edges', () => {});

    it('should be able to delete vertices', () => {});
  });

  describe('graph', () => {
    let newGraph;

    beforeEach(() => {
      newGraph = new Graph();
    });

    it('should be able to insert vertices', () => {
      const node = {value: 'Toast'};
      newGraph.addNode(node);
      expect(newGraph.adjList[node.value].node).to.deep.equal(node);
      expect(newGraph.adjList[node.value].edges).to.deep.equal([]);
    });

    it('should be able to insert vertices more than one', () => {
      const node1 = {value: 'Toast'};
      const node2 = {value: 'Nasi Perang'};
      newGraph.addNode(node1);
      newGraph.addNode(node2);

      expect(newGraph.adjList[node1.value].node).to.deep.equal(node1);
      expect(newGraph.adjList[node1.value].edges).to.deep.equal([]);

      expect(newGraph.adjList[node2.value].node).to.deep.equal(node2);
      expect(newGraph.adjList[node2.value].edges).to.deep.equal([]);
    });

    it('should be able to add edges', () => {
      const node1 = {value: 'Toast'};
      const node2 = {value: 'Nasi Perang'};
      newGraph.addNode(node1);
      newGraph.addNode(node2);

      newGraph.addEdge(node1, node2);
      expect(newGraph.adjList[node1.value].edges).to.deep.equal([node2]);
      expect(newGraph.adjList[node2.value].edges).to.deep.equal([node1]);
    });

    it('should be able to delete edges', () => {
      const node1 = {value: 'Toast'};
      const node2 = {value: 'Nasi Perang'};
      newGraph.addNode(node1);
      newGraph.addNode(node2);

      newGraph.addEdge(node1, node2);
      newGraph.removeEdge(node1, node2);
      expect(newGraph.adjList[node1.value].edges).to.deep.equal([]);
      expect(newGraph.adjList[node2.value].edges).to.deep.equal([]);
    });

    it('should be able to delete vertices', () => {
      const node1 = {value: 'Toast'};
      const node2 = {value: 'Nasi Perang'};
      newGraph.addNode(node1);
      newGraph.addNode(node2);

      newGraph.addEdge(node1, node2);
      newGraph.removeNode(node1);

      expect(newGraph.adjList[node1.value]).to.be.undefined;
      expect(newGraph.adjList[node2.value].edges).to.not.include(node1);
    });

    describe('should be able to traverse', () => {
      let consoleLogStub;

      beforeEach(() => {
        consoleLogStub = sinon.stub(console, 'log');
      });

      afterEach(() => {
        consoleLogStub.restore();
      });

      it('should be able to do depth first search', () => {
        const node1 = {value: 'Toast'};
        const node2 = {value: 'Nasi Perang'};
        const node3 = {value: 'Nasi Goreng'};
        const node4 = {value: 'Indomie'};

        newGraph.addNode(node1);
        newGraph.addNode(node2);
        newGraph.addNode(node3);
        newGraph.addNode(node4);

        newGraph.addEdge(node1, node2);
        newGraph.addEdge(node2, node3);
        newGraph.addEdge(node3, node4);
        newGraph.addEdge(node3, node1);
        newGraph.addEdge(node4, node1);

        newGraph.depthFirstTraversal(node1);
        expect(consoleLogStub).callCount(4);
      });

      it('should be able to do depth first search', () => {
        const node1 = {value: 'Toast'};
        const node2 = {value: 'Nasi Perang'};
        const node3 = {value: 'Nasi Goreng'};
        const node4 = {value: 'Indomie'};

        newGraph.addNode(node1);
        newGraph.addNode(node2);
        newGraph.addNode(node3);
        newGraph.addNode(node4);

        newGraph.addEdge(node1, node2);
        newGraph.addEdge(node2, node3);
        newGraph.addEdge(node3, node4);
        newGraph.addEdge(node3, node1);
        newGraph.addEdge(node4, node1);

        newGraph.breadthFirstTraversal(node1);
        expect(consoleLogStub).callCount(4);
      });
    });
  });

  describe('binary search tree', () => {
    let newBinarySearchTree;

    beforeEach(() => {
      newBinarySearchTree = new BinarySearchTree(6);
    });

    it('should be able to insert to binary search tree', () => {
      newBinarySearchTree.insert(5);
      newBinarySearchTree.insert(7);
      expect(newBinarySearchTree.left.value).to.equal(5);
      expect(newBinarySearchTree.right.value).to.equal(7);
    });

    it('should be able to insert more than one pair to binary search tree', () => {
      const childToCheck1 = newBinarySearchTree.insert(5);
      const childToCheck2 = newBinarySearchTree.insert(7);
      newBinarySearchTree.insert(8);
      newBinarySearchTree.insert(1);

      expect(newBinarySearchTree.left.value).to.equal(5);
      expect(newBinarySearchTree.right.value).to.equal(7);
      expect(childToCheck1.left.value).to.equal(1);
      expect(childToCheck2.right.value).to.equal(8);
    });

    it('should not insert twice', () => {
      newBinarySearchTree.insert(5);
      newBinarySearchTree.insert(7);
      newBinarySearchTree.insert(8);
      newBinarySearchTree.insert(1);
      expect(newBinarySearchTree.insert(1)).to.be.undefined;
    });

    it('should be able to check if binary search tree contains value', () => {
      newBinarySearchTree.insert(5);
      newBinarySearchTree.insert(7);
      newBinarySearchTree.insert(8);
      newBinarySearchTree.insert(1);
      expect(newBinarySearchTree.contains(1)).to.be.true;
    });

    it('should be able to find min', () => {
      newBinarySearchTree.insert(5);
      newBinarySearchTree.insert(7);
      newBinarySearchTree.insert(8);
      newBinarySearchTree.insert(1);
      expect(newBinarySearchTree.min().value).to.equal(1);
    });

    it('should be able to find max', () => {
      newBinarySearchTree.insert(5);
      newBinarySearchTree.insert(7);
      newBinarySearchTree.insert(8);
      newBinarySearchTree.insert(1);
      expect(newBinarySearchTree.max().value).to.equal(8);
    });

    it('should be able remove node with no children', () => {
      const nodeUnderObservation = newBinarySearchTree.insert(2);
      newBinarySearchTree.insert(18);
      newBinarySearchTree.insert(-4);
      newBinarySearchTree.insert(3);

      newBinarySearchTree.remove(-4);
      expect(nodeUnderObservation.left).to.be.null;
    });

    it('should be able remove node with one child', () => {
      newBinarySearchTree.insert(2);
      newBinarySearchTree.insert(18);
      newBinarySearchTree.insert(-4);
      newBinarySearchTree.insert(3);
      newBinarySearchTree.insert(18.5);
      const nodeUnderObservation = newBinarySearchTree.insert(21);
      newBinarySearchTree.insert(19);
      newBinarySearchTree.insert(25);

      newBinarySearchTree.remove(18.5);
      newBinarySearchTree.remove(18);
      expect(newBinarySearchTree.right).to.equal(nodeUnderObservation);
      expect(newBinarySearchTree.right.root).to.equal(newBinarySearchTree);
    });

    it('should be able remove node with two child', () => {
      newBinarySearchTree.insert(2);
      newBinarySearchTree.insert(12);
      newBinarySearchTree.insert(-4);
      newBinarySearchTree.insert(3);
      newBinarySearchTree.insert(9);
      const nodeUnderObservation1 = newBinarySearchTree.insert(21);
      const nodeUnderObservation2 = newBinarySearchTree.insert(19);
      newBinarySearchTree.insert(25);

      newBinarySearchTree.remove(12);

      expect(nodeUnderObservation1.left).to.be.null;

      expect(nodeUnderObservation2.root).to.equal(newBinarySearchTree);
      expect(newBinarySearchTree.right).to.equal(nodeUnderObservation2);

      expect(newBinarySearchTree.right).to.equal(newBinarySearchTree.right.left.root);
      expect(newBinarySearchTree.right).to.equal(newBinarySearchTree.right.right.root);
    });
  });
});