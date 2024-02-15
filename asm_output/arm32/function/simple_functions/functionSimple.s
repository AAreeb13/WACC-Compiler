./valid/function/simple_functions/functionSimple.wacc
calling the reference compiler on ./valid/function/simple_functions/functionSimple.wacc
-- Test: functionSimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function definition and call

# Output:
# 0
#

# Program:

begin
  int f() is
    return 0
  end
  int x = call f() ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionSimple.s contents are:
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
9		@ Stack pointer unchanged, no stack allocated arguments
10		bl wacc_f
11		mov r12, r0
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r12
14		mov r4, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		mov r8, r4
17		mov r0, r8
18		@ statement primitives do not return results (but will clobber r0/rax)
19		bl _printi
20		bl _println
21		@ Stack pointer unchanged, no stack allocated variables
22		mov r0, #0
23		pop {r4, r8, r10, r12}
24		pop {fp, pc}
25	
26	wacc_f:
27		push {fp, lr}
28		mov fp, sp
29		mov r0, #0
30		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
31		mov sp, fp
32		pop {fp, pc}
33		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
34		@ the .ltorg here is used to generate the constant pool after every function
35		@ this is important for ensuring string literals and large integers are accessible!
36		.ltorg
37	
38	@ length of .L._printi_str0
39		.word 2
40	.L._printi_str0:
41		.asciz "%d"
42	.align 4
43	_printi:
44		push {fp, lr}
45		mov fp, sp
46		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
47		bic sp, sp, #0x7
48		mov r1, r0
49		adr r0, .L._printi_str0
50		bl printf
51		mov r0, #0
52		bl fflush
53		mov sp, fp
54		pop {fp, pc}
55	
56	@ length of .L._println_str0
57		.word 0
58	.L._println_str0:
59		.asciz ""
60	.align 4
61	_println:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		adr r0, .L._println_str0
67		bl puts
68		mov r0, #0
69		bl fflush
70		mov sp, fp
71		pop {fp, pc}
===========================================================
-- Finished

