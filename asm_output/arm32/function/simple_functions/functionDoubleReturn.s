./valid/function/simple_functions/functionDoubleReturn.wacc
calling the reference compiler on ./valid/function/simple_functions/functionDoubleReturn.wacc
-- Test: functionDoubleReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with two back-to-back returns at the end.

# Output:
# 3
#

# Program:

begin
    int f() is
        return 3;
        return 5
    end
    int ret = call f();
    println ret
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionDoubleReturn.s contents are:
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
29		mov r0, #3
30		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
31		mov sp, fp
32		pop {fp, pc}
33		mov r0, #5
34		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
35		mov sp, fp
36		pop {fp, pc}
37		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
38		@ the .ltorg here is used to generate the constant pool after every function
39		@ this is important for ensuring string literals and large integers are accessible!
40		.ltorg
41	
42	@ length of .L._printi_str0
43		.word 2
44	.L._printi_str0:
45		.asciz "%d"
46	.align 4
47	_printi:
48		push {fp, lr}
49		mov fp, sp
50		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
51		bic sp, sp, #0x7
52		mov r1, r0
53		adr r0, .L._printi_str0
54		bl printf
55		mov r0, #0
56		bl fflush
57		mov sp, fp
58		pop {fp, pc}
59	
60	@ length of .L._println_str0
61		.word 0
62	.L._println_str0:
63		.asciz ""
64	.align 4
65	_println:
66		push {fp, lr}
67		mov fp, sp
68		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
69		bic sp, sp, #0x7
70		adr r0, .L._println_str0
71		bl puts
72		mov r0, #0
73		bl fflush
74		mov sp, fp
75		pop {fp, pc}
===========================================================
-- Finished

