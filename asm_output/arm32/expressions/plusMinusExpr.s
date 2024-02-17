./valid/expressions/plusMinusExpr.wacc
calling the reference compiler on ./valid/expressions/plusMinusExpr.wacc
-- Test: plusMinusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# +- should be recognised as two separate symbols

# Output:
# -1
#

# Program:

begin
  println 1+-2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
plusMinusExpr.s contents are:
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
9		ldr r9, =-2
10		mov r8, #1
11		adds r8, r8, r9
12		blvs _errOverflow
13		push {r8}
14		pop {r8}
15		mov r8, r8
16		mov r0, r8
17		@ statement primitives do not return results (but will clobber r0/rax)
18		bl _printi
19		bl _println
20		mov r0, #0
21		pop {r8, r10, r12}
22		pop {fp, pc}
23	
24	@ length of .L._prints_str0
25		.word 4
26	.L._prints_str0:
27		.asciz "%.*s"
28	.align 4
29	_prints:
30		push {fp, lr}
31		mov fp, sp
32		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
33		bic sp, sp, #0x7
34		mov r2, r0
35		ldr r1, [r0, #-4]
36		adr r0, .L._prints_str0
37		bl printf
38		mov r0, #0
39		bl fflush
40		mov sp, fp
41		pop {fp, pc}
42	
43	@ length of .L._printi_str0
44		.word 2
45	.L._printi_str0:
46		.asciz "%d"
47	.align 4
48	_printi:
49		push {fp, lr}
50		mov fp, sp
51		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
52		bic sp, sp, #0x7
53		mov r1, r0
54		adr r0, .L._printi_str0
55		bl printf
56		mov r0, #0
57		bl fflush
58		mov sp, fp
59		pop {fp, pc}
60	
61	@ length of .L._println_str0
62		.word 0
63	.L._println_str0:
64		.asciz ""
65	.align 4
66	_println:
67		push {fp, lr}
68		mov fp, sp
69		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
70		bic sp, sp, #0x7
71		adr r0, .L._println_str0
72		bl puts
73		mov r0, #0
74		bl fflush
75		mov sp, fp
76		pop {fp, pc}
77	
78	_exit:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		bl exit
84		mov sp, fp
85		pop {fp, pc}
86	
87	@ length of .L._errOverflow_str0
88		.word 52
89	.L._errOverflow_str0:
90		.asciz "fatal error: integer overflow or underflow occurred\n"
91	.align 4
92	_errOverflow:
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		adr r0, .L._errOverflow_str0
96		bl _prints
97		mov r0, #255
98		bl _exit
===========================================================
-- Finished

