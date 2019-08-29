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