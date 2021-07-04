require 'byebug'
def rotate_rightmost_digits(n, m)
	numbers = n.digits.reverse

	left = numbers.length == m ? [] : numbers[0..(numbers.length - m -1)] #if m == length, numbers[0..-1], which means, returns everything, which is not our expectations. What we want is empty array
	middle = numbers[(numbers.length - m + 1)..-1]
	right = [numbers[numbers.length - m]] # right side use [] because it returns integer, but we need arrays

	numbers =  left + middle + right #type should be all ARRAY to be able to adds

	# Array of integers -> string -> integer
	numbers.join.to_i
end

p rotate_rightmost_digits(735291, 1) == 735291
p rotate_rightmost_digits(735291, 2) == 735219
p rotate_rightmost_digits(735291, 3) == 735912
p rotate_rightmost_digits(735291, 4) == 732915
p rotate_rightmost_digits(735291, 5) == 752913
p rotate_rightmost_digits(735291, 6) == 352917
