./valid/basic/skip/commentInLine.wacc
calling the reference compiler on ./valid/basic/skip/commentInLine.wacc
-- Test: commentInLine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple skip program with in-line comment

# Output:

# Program:

begin
  skip # I can write comments in-line
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
commentInLine.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		movq $0, %rax
8		popq %rbx
9		popq %rbp
10		ret
===========================================================
-- Finished

