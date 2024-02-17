./valid/basic/skip/skip.wacc
calling the reference compiler on ./valid/basic/skip/skip.wacc
-- Test: skip.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple skip program

# Output:

# Program:

begin skip end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
skip.s contents are:
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

