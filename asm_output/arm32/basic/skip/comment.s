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
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		mov r0, #0
9		pop {r8, r10, r12}
10		pop {fp, pc}
===========================================================
-- Finished

