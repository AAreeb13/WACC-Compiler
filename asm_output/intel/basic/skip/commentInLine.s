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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		mov rax, 0
9		pop rbx
10		pop rbp
11		ret
===========================================================
-- Finished

