# Written by Zach Allwein for CS2340.001, assignment 2, starting Sep 18, 2024.
# NetID: zra22000
# This program repeatedly prompts a user to enter a number, manually converts that number from a string to a binary number,
# and keeps track of the running total. It includes error handling for non-number and malformed input
# tracking the number of valid and invalid inputs to be output at the end as program statistics
# When the user enters a newline the program terminates printing the running total, number of errors, and number of successes

                  # declaration of needed services with identifiers for clarity
                  .eqv SysPrintInt 	  1
	                .eqv SysPrintString	4
	                .eqv SysReadString	8
	                .eqv SysExit		    10

                  .data
buffer:           .space   16                                             # buffer to hold number
capacity:         .word    16                                             # length of the buffer
enter_num:        .asciiz "Enter a number: "                              # prompt for number to convert
sum:              .asciiz "The sum of the entered numbers is: "           # sum summary statistic text
errors:           .asciiz "\nThe total number of errors encountered is: " # number of errors summary statistic text
valids:           .asciiz "\nThe number of valid numbers entered is: "    # number of valid inputs summary statistic text

# code section
                  .text
main:             li $s0, 0 # initiialize s0 register as accumulator
                  li $s1, 0 # initiazlie s1 register to hold number of valid numbers entered
                  li $s2, 0 # initialize s2 register to hold number of errors encountered

prompt:           li $v0, SysPrintString # prompt the user for a number
                  la $a0, enter_num      # load the address of the text prompt
                  syscall                # call print string service

                  li $v0, SysReadString # read user input into buffer
                  la $a0, buffer        # load address of the buffer to input into as arg
                  lw $a1, capacity      # load capacity of buffer as argument
                  syscall               # call read string service

                  move $s4, $a0 # store buffer address in s1

                  lb $t0, 0($s4)    # load first char in buffer to t1
                  li $t1, '\n'      # load newline t1
                  beq $t0, $t1, end # branch to end instruction if first byte in buffer is newline signaling termination

                  li $s5, 0         # initialize s5 register to hold an is negative flag
                  li $t1, '-'       # load minus sign t1
                  seq $s5, $t0, $t1 # set s5 to 1 if first byte in buffer is minus sign

                  # conditionally get rid of - sign if number is negative
                  add $s4, $s4, $s5 # add offset to buffer address

                  # if current byte in buffer is '\n' user entered malfromed input, handle errors accodingly
                  lb $t0, 0($s4)              # load current char in buffer to t1
                  li $t1, '\n'                # load newline t1
                  beq $t0, $t1, convert_error # branch to end if first byte in buffer is newline

                  # convert to binary number
                  li $s6, 0 # initialize s6 to hold conversion accumulator

convert_loop:     lb $t0, 0($s4)            # load current char in buffer to t1
                  li $t1, '\n'              # load newline t1
                  beq $t0, $t1, end_convert # branch to end_convert if current byte in buffer is newline

                  li $t1, 10        # load scale factor for accumulator
                  mul $s6, $s6, $t1 # scale accumulator by 10

                  subi $t0, $t0, '0' # subtract 0 to convert from ascii to digit

                  bgeu $t0, $t1, convert_error # if number is not 0-9 handle errors

                  add $s6, $s6, $t0 # add digit to accumulation

                  addi $s4, $s4, 1 # increment buffer pointer
                  b convert_loop   # continue parsing number

convert_error:    addi $s2, $s2, 1 # increment number of errors
                  b prompt         # go back to the prompt step

                  # convert positive binary num to neagtive using 2's complements rules
convert_negative: not  $s6, $s6     # invert the bits of parsed number
                  addi $s6, $s6, 1 # add one
                  li   $s5, 0        # set is negative flag to 0

end_convert:      bnez $s5, convert_negative # if the is negative flag is set branch to convert negative
                  addi $s1, $s1, 1           # increment number of succesfully parsed numbers
                  add $s0, $s0, $s6          # add parsed number to the accumulation
                  beqz $s5, prompt           # go back to prompt and allow user to keep entering numbers

end:              li $v0, SysPrintString # load print string service
                  la $a0, sum            # load address of sum sumary statistic
                  syscall                # ouput sum summary statistic prefix
                  li $v0, SysPrintInt    # load print int service
                  move $a0, $s0          # move the accumulated total to argument register
                  syscall                # ouput sum summary statistic

                  li $v0, SysPrintString # load print string service
                  la $a0, valids         # load address of number of valid inputs summary statistic
                  syscall                # ouput summary statistic prefix
                  li $v0, SysPrintInt    # load print int service
                  move $a0, $s1          # move number of valid inputs into argument register
                  syscall                # ouput valids summary statistic

                  li $v0, SysPrintString # print number of errors
                  la $a0, errors         # load address of errors summary statistic
                  syscall                # ouput errors summary statistic prefix
                  li $v0, SysPrintInt    # load print int service
                  move $a0, $s2          # move number of erros into argument register
                  syscall                # ouput errors summary statistic

                  li $v0, SysExit # load sys exit service
                  syscall         # exit the program
