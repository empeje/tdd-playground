import { expect } from 'chai';
import { describe } from 'mocha'
import {
  cachedTimes10,
  memoizeClosureTimes10,
  memoizeClosureTimesM,
  memoize,
  factorial,
  joinElements,
  joinElementsIterative,
  joinElementsMemoized,
  isUnique,
  isUniqueBreadcrumb,
  uniqSort,
  linearSearch,
  binarySearch,
  bubbleSort,
  bubbleSortWhile,
  min,
  selectionSort,
  insertionSort,
  mergeSort
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

    describe('memoizeClosureTimesM', () => {
      it('should calculate times M correctly when M = 10', () => {
        const calculator = memoizeClosureTimesM(10);
        expect(calculator(1)).to.equal(10);
        expect(calculator(1)).to.equal(10);
      });

      it('should calculate times M correctly when M = 12', () => {
        const calculator = memoizeClosureTimesM(12);
        expect(calculator(1)).to.equal(12);
        expect(calculator(1)).to.equal(12);
      });
    });

    describe('generic memoize', () => {
      it('should memoize any function', () => {
        const times10 = n => n * 10;
        const calculator = memoize(times10);
        expect(calculator(9)).to.equal(90);
        expect(calculator(9)).to.equal(90);
      });
    });
  });

  describe('recursion', () => {
    describe('recursive', () => {
      it('should return correct results', () => {
        expect(joinElements(['s', 'cr', 't cod', ' :) :)', 'e'], 'e')).to.equal('secret code :) :)')
      });
    });

    describe('memoize', () => {
      it('should return correct results', () => {
        expect(joinElementsMemoized(['s', 'cr', 't cod', ' :) :)', 'e'], 'e')).to.equal('secret code :) :)')
      });
    });

    describe('iterative', () => {
      it('should return correct results', () => {
        expect(joinElementsIterative(['s', 'cr', 't cod', ' :) :)', 'e'], 'e')).to.equal('secret code :) :)')
      });
    });

    describe('factorial', () => {
      describe('recursive', () => {
        it('should return correct results', () => {
          expect(factorial(5)).to.equal(120);
        })
      });

      describe('memoize', () => {
        it('should return correct results', () => {
          const calculator = memoize(factorial);
          expect(calculator(5)).to.equal(120);
          expect(calculator(5)).to.equal(120);
        })
      });
    });
  });

  describe('divide and conquer', () => {
    describe('linear search', () => {
      it('should return correct results', () => {
        expect(linearSearch([2,6,7,90,103], 90))
          .to.deep.equal({ error: false, index: 3, value: 90 });
      });

      it('should return error not found when item not in the list', () =>{
        expect(linearSearch([2,6,7,90,103], 1))
          .to.deep.equal({ error: true, index: -1 });
      });
    });

    describe('binary search', () => {
      it('should return correct results', () => {
        expect(binarySearch([2,6,7,90,103], 90)).to.equal(3);
      });

      it('should return error not found when item not in the list', () =>{
        expect(binarySearch([2,6,7,90,103], 1)).to.equal(-1);
      });
    });

    describe('bubble sort', () => {
      it('should return correct results', () => {
        expect(bubbleSort([5,1,4,2,8])).to.deep.equal([1,2,4,5,8]);
        expect(bubbleSort([33,3,5,5,2,34,234,2,3,134,14,3])).to.deep.equal( [33,3,5,5,2,34,234,2,3,134,14,3].sort((a,b) => a-b));
      })
    });

    describe('bubble sort while loop', () => {
      it('should return correct results', () => {
        expect(bubbleSortWhile([5,1,4,2,8])).to.deep.equal([1,2,4,5,8]);
        expect(bubbleSortWhile([33,3,5,5,2,34,234,2,3,134,14,3])).to.deep.equal( [33,3,5,5,2,34,234,2,3,134,14,3].sort((a,b) => a-b));
      })
    });

    describe('min', () => {
      it('should return correct results', () => {
       expect(min([3,5,6,4,3,1,1,34,5,6])).to.deep.equal({ value: 1, index: 5 });
      });
    });

    describe('selection sort', () => {
      it('should return correct results', () => {
        expect(selectionSort([5,1,4,2,8])).to.deep.equal([1,2,4,5,8]);
        expect(selectionSort([33,3,5,5,2,34,234,2,3,134,14,3])).to.deep.equal( [33,3,5,5,2,34,234,2,3,134,14,3].sort((a,b) => a-b));
      })
    });

    describe('insertion sort', () => {
      it('should return correct results', () => {
        expect(insertionSort([5,1,4,2,8])).to.deep.equal([1,2,4,5,8]);
        expect(insertionSort([33,3,5,5,2,34,234,2,3,134,14,3])).to.deep.equal( [33,3,5,5,2,34,234,2,3,134,14,3].sort((a,b) => a-b));
      });
    });

    describe('merge sort', () => {
      it('should return correct results', () => {
        expect(mergeSort([5,1,4,2,8])).to.deep.equal([1,2,4,5,8]);
        expect(mergeSort([33,3,5,5,2,34,234,2,3,134,14,3])).to.deep.equal( [33,3,5,5,2,34,234,2,3,134,14,3].sort((a,b) => a-b));
      });
    });
  })
});