import { expect } from 'chai';
import { describe, beforeEach } from 'mocha'
import {
  Stack,
  Queue,
  LinkedList,
  HashTable
} from "../src/dataStructure";

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

    it('should be able to resize', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.insert(testData[1].key, testData[1].value);
      newHashTable.resize(75);

      expect(newHashTable._size).to.equal(75);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[0].value)).to.deep.equal([[testData[0].key, testData[0].value]]);
      expect(newHashTable._storage
        .filter(value => value !== undefined)
        .find(value => value[0][1] === testData[1].value)).to.deep.equal([[testData[1].key, testData[1].value]]);
    });

    it('should be able to remove', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      newHashTable.remove(testData[0].key);
      expect(newHashTable._storage.find(value => value === testData[0].value)).to.be.undefined;
    });

    it('should be able to retrieve', () => {
      newHashTable.insert(testData[0].key, testData[0].value);
      expect(newHashTable.retrieve(testData[0].key)).to.deep.equal(testData[0].value);
    });
  });
});