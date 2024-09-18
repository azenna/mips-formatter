# Compute the first twelve Fibonacci numbers and put in array, then print.
# This is an example from a Web site, commented and modified by John Cole.
#
      	.include "SysCalls.asm"
     	.data
fibs: 	.word   0 : 12        # "array" of 12 words to contain fib values
size: 	.word  12             # size of "array"

# Start of code.
      	lw   	$t5, 0($t5)      # load array size
	la   	$t0, fibs        # load address of array
	la   	$t5, size        # load address of size variable
      	lw   	$t5, 0($t5)      # load array size
      	li   	$t2, 1           # 1 is first and second Fib. number
