import { expect } from 'chai';
import { describe, beforeEach } from 'mocha'
import {
  Stack,
  Queue
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
    })
  });

  describe('linked list', () => {

  });

  describe('hash table', () => {

  });
});