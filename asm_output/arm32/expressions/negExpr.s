./valid/expressions/negExpr.wacc
calling the reference compiler on ./valid/expressions/negExpr.wacc
-- Test: negExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating negation

# Output:
# -42
#

# Program:

begin
	int x = 42 ;
  println -x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negExpr.s contents are:
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
9		mov r8, #42
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		rsbs r8, r4, #0
13		blvs _errOverflow
14		push {r8}
15		pop {r8}
16		mov r8, r8
17		mov r0, r8
18		@ statement primitives do not return results (but will clobber r0/rax)
19		bl _printi
20		bl _println
21		@ Stack pointer unchanged, no stack allocated variables
22		mov r0, #0
23		pop {r4, r8, r10, r12}
24		pop {fp, pc}
25	
26	@ length of .L._prints_str0
27		.word 4
28	.L._prints_str0:
29		.asciz "%.*s"
30	.align 4
31	_prints:
32		push {fp, lr}
33		mov fp, sp
34		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
35		bic sp, sp, #0x7
36		mov r2, r0
37		ldr r1, [r0, #-4]
38		adr r0, .L._prints_str0
39		bl printf
40		mov r0, #0
41		bl fflush
42		mov sp, fp
43		pop {fp, pc}
44	
45	@ length of .L._printi_str0
46		.word 2
47	.L._printi_str0:
48		.asciz "%d"
49	.align 4
50	_printi:
51		push {fp, lr}
52		mov fp, sp
53		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
54		bic sp, sp, #0x7
55		mov r1, r0
56		adr r0, .L._printi_str0
57		bl printf
58		mov r0, #0
59		bl fflush
60		mov sp, fp
61		pop {fp, pc}
62	
63	@ length of .L._println_str0
64		.word 0
65	.L._println_str0:
66		.asciz ""
67	.align 4
68	_println:
69		push {fp, lr}
70		mov fp, sp
71		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
72		bic sp, sp, #0x7
73		adr r0, .L._println_str0
74		bl puts
75		mov r0, #0
76		bl fflush
77		mov sp, fp
78		pop {fp, pc}
79	
80	_exit:
81		push {fp, lr}
82		mov fp, sp
83		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
84		bic sp, sp, #0x7
85		bl exit
86		mov sp, fp
87		pop {fp, pc}
88	
89	@ length of .L._errOverflow_str0
90		.word 52
91	.L._errOverflow_str0:
92		.asciz "fatal error: integer overflow or underflow occurred\n"
93	.align 4
94	_errOverflow:
95		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
96		bic sp, sp, #0x7
97		adr r0, .L._errOverflow_str0
98		bl _prints
99		mov r0, #255
100		bl _exit
===========================================================
-- Finished

