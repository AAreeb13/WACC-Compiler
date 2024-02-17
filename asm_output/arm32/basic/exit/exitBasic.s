./valid/basic/exit/exitBasic.wacc
calling the reference compiler on ./valid/basic/exit/exitBasic.wacc
-- Test: exitBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic exit statement

# Output:

# Exit:
# 7

# Program:

begin
  exit 7
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exitBasic.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated arguments
9		mov r8, #7
10		mov r0, r8
11		@ statement primitives do not return results (but will clobber r0/rax)
12		bl _exit
13		mov r0, #0
14		pop {r8, r10, r12}
15		pop {fp, pc}
16	
17	_exit:
18		push {fp, lr}
19		mov fp, sp
20		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
21		bic sp, sp, #0x7
22		bl exit
23		mov sp, fp
24		pop {fp, pc}
===========================================================
-- Finished

