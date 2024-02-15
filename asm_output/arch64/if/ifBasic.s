./valid/if/ifBasic.wacc
calling the reference compiler on ./valid/if/ifBasic.wacc
-- Test: ifBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple if statement

# Output:

# Program:

begin
  if true
  then
    skip
  else
    skip
  fi
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifBasic.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		mov x8, #1
9		cmp x8, #1
10		b.eq .L0
11		b .L1
12	.L0:
13	.L1:
14		mov x0, #0
15		// pop {fp, lr}
16		ldp fp, lr, [sp], #16
17		ret
===========================================================
-- Finished

