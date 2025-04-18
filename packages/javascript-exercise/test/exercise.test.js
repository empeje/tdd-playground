import { expect } from 'chai';
import { describe, beforeEach } from 'mocha'
import {
  diffBetweenTwoStrings,
  root,
} from "../src/exercise";

describe('exercise test', () => {
  describe('string exercises', () => {
    describe('string diff', () => {
      it('should be able to diff two strings', () => {
        expect(diffBetweenTwoStrings('ABCDEFG', 'ABDFFGH')).to.deep.equal(["A", "B", "-C", "D", "-E", "F", "+F", "G", "+H"])
      });
    });
  });

  describe('search algorithm', () => {
    describe('finding root', () => {
      it('should return correct result', () => {
        expect(root(25, 2)).to.approximately(5, 0.001);
        expect(root(169, 2)).to.approximately(13, 0.001);
        expect(root(10000, 2)).to.approximately(100, 0.001);
        expect(root(1000000, 3)).to.approximately(100, 0.001);
      });
    })
  });
});