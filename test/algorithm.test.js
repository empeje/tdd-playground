import { expect } from 'chai';
import { describe } from 'mocha'
import {
  cachedTimes10,
  memoizeClosureTimes10,
  isUnique,
  isUniqueBreadcrumb,
  uniqSort
} from "../src";

describe('algorithms', () => {
  describe('isUnique', () => {
    it('should return true when unique', () => {
      expect(isUnique([1,2,3])).to.equal(true);
    });

    it('should return false when not unique', () => {
      expect(isUnique([1,2,2])).to.equal(false);
    });
  });

  describe('isUniqueBreadcrumb', () => {
    it('should return true when unique', () => {
      expect(isUniqueBreadcrumb([1,2,3])).to.equal(true);
    });

    it('should return false when not unique', () => {
      expect(isUniqueBreadcrumb([1,2,2])).to.equal(false);
    });
  });

  describe('uniqSort', () => {
    it('should not return any duplicate values in the sorted array', () => {
      expect(uniqSort([4,2,2,3,2,2,2])).to.deep.equal([2,3,4]);
     expect(uniqSort([1,5,4,1,1])).to.deep.equal([1,4,5]);
    });
  });

  describe('times10', () => {
    describe('cachedTimes10', () => {
      it('should calculate times 10 correctly', () => {
        expect(cachedTimes10(1)).to.equal(10);
        expect(cachedTimes10(1)).to.equal(10);
      });
    });

    describe('memoizeClosureTimes10', () => {
      it('should calculate times 10 correctly', () => {
        const calculator = memoizeClosureTimes10();
        expect(calculator(1)).to.equal(10);
        expect(calculator(1)).to.equal(10);
      });
    });
  });
});