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
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		mov r8, #1
9		cmp r8, #1
10		beq .L0
11		b .L1
12	.L0:
13	.L1:
14		mov r0, #0
15		pop {r8, r10, r12}
16		pop {fp, pc}
===========================================================
-- Finished

