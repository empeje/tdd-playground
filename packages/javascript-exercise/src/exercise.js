export const diffBetweenTwoStrings = (source, target) => {
  /**
   @param source: string
   @param target: string
   @return: string[]
   */
  const results = [];
  let i = 0;
  let j = 0;

  while (i < source.length || j < target.length) {
    if (i < source.length && j < target.length && source[i] === target[j]) {
      // Characters match - keep them
      results.push(source[i]);
      i++;
      j++;
    } else {
      // Look ahead to see if we can find a match
      let foundMatch = false;
      if (i < source.length && j < target.length) {
        // Check next character in target
        if (source[i] === target[j + 1]) {
          results.push(`+${target[j]}`);
          j++;
          continue;
        }
        // Check next character in source
        if (source[i + 1] === target[j]) {
          results.push(`-${source[i]}`);
          i++;
          continue;
        }
      }
      
      // No match found, handle remaining characters
      if (i < source.length) {
        results.push(`-${source[i]}`);
        i++;
      }
      if (j < target.length) {
        results.push(`+${target[j]}`);
        j++;
      }
    }
  }

  return results;
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
