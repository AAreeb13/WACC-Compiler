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
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// Stack pointer unchanged, no stack allocated arguments
12		bl wacc_f
13		mov x16, x0
14		// Stack pointer unchanged, no stack allocated arguments
15		mov x8, x16
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		mov x8, x19
19		mov x0, x8
20		// statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		// Stack pointer unchanged, no stack allocated variables
24		mov x0, #0
25		// pop {x19}
26		ldp x19, xzr, [sp], #16
27		// pop {fp, lr}
28		ldp fp, lr, [sp], #16
29		ret
30	
31	wacc_f:
32		// push {fp, lr}
33		stp fp, lr, [sp, #-16]!
34		mov fp, sp
35		mov x0, #3
36		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
37		mov sp, fp
38		// pop {fp, lr}
39		ldp fp, lr, [sp], #16
40		ret
41		mov x0, #5
42		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
43		mov sp, fp
44		// pop {fp, lr}
45		ldp fp, lr, [sp], #16
46		ret
47		// 'ere be dragons: this is 100% dead code, functions always end in returns!
48	
49	// length of .L._printi_str0
50		.word 2
51	.L._printi_str0:
52		.asciz "%d"
53	.align 4
54	_printi:
55		// push {lr}
56		stp lr, xzr, [sp, #-16]!
57		mov x1, x0
58		adr x0, .L._printi_str0
59		bl printf
60		mov x0, #0
61		bl fflush
62		// pop {lr}
63		ldp lr, xzr, [sp], #16
64		ret
65	
66	// length of .L._println_str0
67		.word 0
68	.L._println_str0:
69		.asciz ""
70	.align 4
71	_println:
72		// push {lr}
73		stp lr, xzr, [sp, #-16]!
74		adr x0, .L._println_str0
75		bl puts
76		mov x0, #0
77		bl fflush
78		// pop {lr}
79		ldp lr, xzr, [sp], #16
80		ret
===========================================================
-- Finished

