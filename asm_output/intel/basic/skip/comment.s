./valid/basic/skip/comment.wacc
calling the reference compiler on ./valid/basic/skip/comment.wacc
-- Test: comment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple skip program with comment line

# Output:

# Program:

begin
  # I can write stuff on a comment line
  skip
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
comment.s contents are:
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

