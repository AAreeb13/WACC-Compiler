./valid/expressions/minusExpr.wacc
calling the reference compiler on ./valid/expressions/minusExpr.wacc
-- Test: minusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating subtraction

# Output:
# 5
#

# Program:

begin
  int x = 15 ;
  int y = 20 ;
  println y - x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
minusExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #15
10		mov r4, r8
11		mov r8, #20
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		subs r8, r5, r4
15		blvs _errOverflow
16		push {r8}
17		pop {r8}
18		mov r8, r8
19		mov r0, r8
20		@ statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		@ Stack pointer unchanged, no stack allocated variables
24		mov r0, #0
25		pop {r4, r5, r8, r10, r12}
26		pop {fp, pc}
27	
28	@ length of .L._prints_str0
29		.word 4
30	.L._prints_str0:
31		.asciz "%.*s"
32	.align 4
33	_prints:
34		push {fp, lr}
35		mov fp, sp
36		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
37		bic sp, sp, #0x7
38		mov r2, r0
39		ldr r1, [r0, #-4]
40		adr r0, .L._prints_str0
41		bl printf
42		mov r0, #0
43		bl fflush
44		mov sp, fp
45		pop {fp, pc}
46	
47	@ length of .L._printi_str0
48		.word 2
49	.L._printi_str0:
50		.asciz "%d"
51	.align 4
52	_printi:
53		push {fp, lr}
54		mov fp, sp
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		mov r1, r0
58		adr r0, .L._printi_str0
59		bl printf
60		mov r0, #0
61		bl fflush
62		mov sp, fp
63		pop {fp, pc}
64	
65	@ length of .L._println_str0
66		.word 0
67	.L._println_str0:
68		.asciz ""
69	.align 4
70	_println:
71		push {fp, lr}
72		mov fp, sp
73		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
74		bic sp, sp, #0x7
75		adr r0, .L._println_str0
76		bl puts
77		mov r0, #0
78		bl fflush
79		mov sp, fp
80		pop {fp, pc}
81	
82	_exit:
83		push {fp, lr}
84		mov fp, sp
85		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
86		bic sp, sp, #0x7
87		bl exit
88		mov sp, fp
89		pop {fp, pc}
90	
91	@ length of .L._errOverflow_str0
92		.word 52
93	.L._errOverflow_str0:
94		.asciz "fatal error: integer overflow or underflow occurred\n"
95	.align 4
96	_errOverflow:
97		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
98		bic sp, sp, #0x7
99		adr r0, .L._errOverflow_str0
100		bl _prints
101		mov r0, #255
102		bl _exit
===========================================================
-- Finished

