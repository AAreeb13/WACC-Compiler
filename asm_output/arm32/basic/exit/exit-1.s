./valid/basic/exit/exit-1.wacc
calling the reference compiler on ./valid/basic/exit/exit-1.wacc
-- Test: exit-1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# common error exit statement

# Output:

# Exit:
# 255

# Program:

begin
  exit -1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exit-1.s contents are:
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
9		ldr r8, =-1
10		mov r8, r8
11		mov r0, r8
12		@ statement primitives do not return results (but will clobber r0/rax)
13		bl _exit
14		mov r0, #0
15		pop {r8, r10, r12}
16		pop {fp, pc}
17	
18	_exit:
19		push {fp, lr}
20		mov fp, sp
21		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
22		bic sp, sp, #0x7
23		bl exit
24		mov sp, fp
25		pop {fp, pc}
===========================================================
-- Finished

