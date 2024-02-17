./valid/variables/longVarNames.wacc
calling the reference compiler on ./valid/variables/longVarNames.wacc
-- Test: longVarNames.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable can have very long names

# Output:

# Exit:
# 5

# Program:

begin
  int this_variable_has_a_crazy_long_name_but_ought_to_still_be_valid = 5 ;
  exit this_variable_has_a_crazy_long_name_but_ought_to_still_be_valid
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longVarNames.s contents are:
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
9		mov r8, #5
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _exit
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r0, #0
18		pop {r4, r8, r10, r12}
19		pop {fp, pc}
20	
21	_exit:
22		push {fp, lr}
23		mov fp, sp
24		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
25		bic sp, sp, #0x7
26		bl exit
27		mov sp, fp
28		pop {fp, pc}
===========================================================
-- Finished

