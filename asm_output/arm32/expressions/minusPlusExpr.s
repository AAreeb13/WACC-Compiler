./valid/expressions/minusPlusExpr.wacc
calling the reference compiler on ./valid/expressions/minusPlusExpr.wacc
-- Test: minusPlusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# -+ should be recognised as two separate symbols

# Output:
# -1
#

# Program:

begin
  println 1-+2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
minusPlusExpr.s contents are:
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
9		mov r8, #1
10		subs r8, r8, #2
11		blvs _errOverflow
12		push {r8}
13		pop {r8}
14		mov r8, r8
15		mov r0, r8
16		@ statement primitives do not return results (but will clobber r0/rax)
17		bl _printi
18		bl _println
19		mov r0, #0
20		pop {r8, r10, r12}
21		pop {fp, pc}
22	
23	@ length of .L._prints_str0
24		.word 4
25	.L._prints_str0:
26		.asciz "%.*s"
27	.align 4
28	_prints:
29		push {fp, lr}
30		mov fp, sp
31		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
32		bic sp, sp, #0x7
33		mov r2, r0
34		ldr r1, [r0, #-4]
35		adr r0, .L._prints_str0
36		bl printf
37		mov r0, #0
38		bl fflush
39		mov sp, fp
40		pop {fp, pc}
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
76	
77	_exit:
78		push {fp, lr}
79		mov fp, sp
80		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
81		bic sp, sp, #0x7
82		bl exit
83		mov sp, fp
84		pop {fp, pc}
85	
86	@ length of .L._errOverflow_str0
87		.word 52
88	.L._errOverflow_str0:
89		.asciz "fatal error: integer overflow or underflow occurred\n"
90	.align 4
91	_errOverflow:
92		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
93		bic sp, sp, #0x7
94		adr r0, .L._errOverflow_str0
95		bl _prints
96		mov r0, #255
97		bl _exit
===========================================================
-- Finished

