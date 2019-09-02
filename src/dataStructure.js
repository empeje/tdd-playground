/** Class representing a Stack. */
class Stack {

  constructor() {
    this._storage = {};
    this._size = 0;
  }
  /*
  * Adds a new value at the end of the stack
  * @param {*} value the value to push
  */
  push(value) {
    this._size ++;
    this._storage[this._size] = value;
  }

  /*
  * Removes the value at the end of the stack and returns it
  * @return {*} the last and newest value in the stack
  */
  pop() {
    if(this._size){
      const results = this._storage[this._size];
      delete this._storage[this._size];
      this._size --;
      return results;
    }
  }
  /*
  * Returns the value at the end of the stack without removing it
  * @return {*} the last and newest value in the stack
  */
  peek() {
    if(this._size){
      return this._storage[this._size];
    }
  }

  size() {
    return this._size;
  }

  empty() {
    this._storage = {};
    this._size = 0;
  }

  swap() {
    const temp = this._storage[this._size];
    this._storage[this._size] = this._storage[this._size - 1];
    this._storage[this._size - 1] = temp;
  }
}

export {
  Stack
}