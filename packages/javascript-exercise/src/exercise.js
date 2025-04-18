export const diffBetweenTwoStrings = (source, target) => {
  /**
   @param source: string
   @param target: string
   @return: string[]
   */

  const results = [];

  const sourceStack = source.split('').reverse();
  const targetStack = target.split('').reverse();

  let index = 0;
  // while(sourceStack.length > 0 || targetStack.length > 0) {
  //   if(sourceStack[index] === targetStack[index]){
  //     results.push(sourceStack[index]);
  //     sourceStack.pop();
  //     targetStack.pop();
  //   } else {
  //
  //   }
  //
  //   index++;
  // }
};

export const root = (base, exponent) => {
  const updateGuess = (top, bottom) => bottom + (top - bottom) / 2;
  const updateGuessToThePowerOfN = guess => Math.pow(guess, exponent);

  const accuracy = 0.001;
  let lowerBound = base - accuracy;
  let upperBound = base + accuracy;

  let top = base;
  let bottom = 0;

  let guess = updateGuess(top, bottom);
  let guessToThePowerOfN = Math.pow(guess, exponent);

  while(!(lowerBound <= guessToThePowerOfN && guessToThePowerOfN <= upperBound)) {
    if(guessToThePowerOfN > upperBound) {
      top = guess - accuracy;
    } else if (lowerBound > guessToThePowerOfN) {
      bottom = guess + accuracy;
    }

    guess = updateGuess(top, bottom);
    guessToThePowerOfN = updateGuessToThePowerOfN(guess);
  }

  return guess;
};
