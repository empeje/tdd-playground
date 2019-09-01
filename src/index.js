export const isUnique = (arr) => {
  let result = true;

  for (let i = 0; i < arr.length; i++) {
    for(let j = 0; j < arr.length; j++) {
      if(i !== j && arr[i] === arr[j]) {
        result = false;
      }
    }
  }

  return result;
};

export const isUniqueBreadcrumb = (arr) => {
  const breadcrumbs = {};
  let result = true;

  for (let i = 0; i < arr.length; i++) {
   if(breadcrumbs[arr[i]]) {
     result = false;
   } else {
     breadcrumbs[arr[i]] = true;
   }
  }

  return result;
};

// Task: Transform this simple sorting algorithm into a unique sort.
// It should not return any duplicate values in the sorted array.
export const uniqSort = (arr) => {
  const breadcrumbs = {};
  const uniqArr = [];

  for (let i = 0; i < arr.length; i++) {
    if(!breadcrumbs[arr[i]]) {
      breadcrumbs[arr[i]] = true;
      uniqArr.push(arr[i]);
    }
  }

  return uniqArr.sort((a,b) => a - b)
};

const times10cache = {};
export const cachedTimes10 = number => {
  if(!times10cache[number]){
    console.log('this code should not be executed if result exist');
    times10cache[number] = number * 10;
  }
  return times10cache[number];
};

export const times10 = n => n * 10;

export const memoizeClosureTimes10 = () => {
  const cache = {};

   return number => {
    if(!cache[number]){
      console.log('this code should not be executed if result exist');
      cache[number] = times10(number);
    }
    return cache[number];
  };
};

export const memoizeClosureTimesM = m => {
  const cache = {};

  return number => {
    if(!cache[number]){
      console.log('this code should not be executed if result exist');
      cache[number] = number * m;
    }
    return cache[number];
  };
};

export const memoize = callback => {
  const cache = {};

  return (...args) => {
    const argsHash = JSON.stringify({...args});
    if(!cache[argsHash]){
      console.log('INFO: updating cache');
      cache[argsHash] = callback(args);
    }
    console.log('INFO: using cache');
    return cache[argsHash];
  };
};

export const joinElements = (array, joinString) => {
  const recurse = (index, resultSoFar) => {
    resultSoFar += array[index];

    if(index === array.length - 2) {
      return resultSoFar;
    } else {
      return recurse(index + 1, resultSoFar + joinString)
    }
  };

  return recurse(0, '');
};

export const joinElementsIterative = (array, joinString) => {
  let resultSoFar = '';

  for(let index = 0; index < array.length - 1; index++){
    resultSoFar += array[index];
    if(index !== array.length - 2) resultSoFar += joinString;
  }
  return resultSoFar;
};

export const joinElementsMemoized = (array, joinString, resultSoFar = '', index = 0) => {
  resultSoFar += array[index];
  if(index === array.length - 2) return resultSoFar;
  return joinElementsMemoized(array, joinString, resultSoFar + joinString, index + 1) ;
};

export const factorial = number => {
  if (number <= 1) return 1;
  return number * factorial(number-1);
};

export const linearSearch = (list, item) => {
  for(let i = 0 ; i < list.length; i++) {
    if (list[i] === item) {
      return { error: false, index: i, value: item }
    }
  }
  return { error: true, index: -1 }
};
