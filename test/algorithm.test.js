import { expect } from 'chai';
import { describe } from 'mocha'
import { isUnique, isUniqueBreadcrumb, uniqSort } from "../src";

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
});