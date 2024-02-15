./valid/sequence/intAssignment.wacc
calling the reference compiler on ./valid/sequence/intAssignment.wacc
-- Test: intAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer variable declaration and assignment

# Output:

# Exit:
# 20

# Program:

begin
  int x = 10 ;
  x = 20 ;
  exit x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intAssignment.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #10
10		mov r4, r8
11		mov r8, #20
12		mov r4, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		mov r8, r4
15		mov r0, r8
16		@ statement primitives do not return results (but will clobber r0/rax)
17		bl _exit
18		@ Stack pointer unchanged, no stack allocated variables
19		mov r0, #0
20		pop {r4, r8, r10, r12}
21		pop {fp, pc}
22	
23	_exit:
24		push {fp, lr}
25		mov fp, sp
26		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
27		bic sp, sp, #0x7
28		bl exit
29		mov sp, fp
30		pop {fp, pc}
===========================================================
-- Finished

