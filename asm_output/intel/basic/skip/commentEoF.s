./valid/basic/skip/commentEoF.wacc
calling the reference compiler on ./valid/basic/skip/commentEoF.wacc
-- Test: commentEoF.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple skip program with comment line ended by EoF not EoL

# Output:

# Program:

begin 
  skip 
end
# I can end a file with a comment
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
commentEoF.s contents are:
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

