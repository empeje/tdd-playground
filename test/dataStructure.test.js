import { expect } from 'chai';
import { describe, beforeEach } from 'mocha'
import {
  Stack
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
    it('should be able to enqueue', () => {

    });

    it('should be able to dequeue', () => {

    });
  });

  describe('linked list', () => {

  });

  describe('hash table', () => {

  });
});