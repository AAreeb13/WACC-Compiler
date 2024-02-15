./valid/expressions/intCalc.wacc
calling the reference compiler on ./valid/expressions/intCalc.wacc
-- Test: intCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer calculation

# Output:
# 72
#

# Program:

begin
  int x = 42 ;
  int y = 30 ;
  int z = x + y ;
  println z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intCalc.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #42
10		mov r4, r8
11		mov r8, #30
12		mov r5, r8
13		adds r8, r4, r5
14		blvs _errOverflow
15		push {r8}
16		pop {r8}
17		mov r8, r8
18		mov r6, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, r6
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		@ Stack pointer unchanged, no stack allocated variables
26		mov r0, #0
27		pop {r4, r5, r6, r8, r10, r12}
28		pop {fp, pc}
29	
30	@ length of .L._prints_str0
31		.word 4
32	.L._prints_str0:
33		.asciz "%.*s"
34	.align 4
35	_prints:
36		push {fp, lr}
37		mov fp, sp
38		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
39		bic sp, sp, #0x7
40		mov r2, r0
41		ldr r1, [r0, #-4]
42		adr r0, .L._prints_str0
43		bl printf
44		mov r0, #0
45		bl fflush
46		mov sp, fp
47		pop {fp, pc}
48	
49	@ length of .L._printi_str0
50		.word 2
51	.L._printi_str0:
52		.asciz "%d"
53	.align 4
54	_printi:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		mov r1, r0
60		adr r0, .L._printi_str0
61		bl printf
62		mov r0, #0
63		bl fflush
64		mov sp, fp
65		pop {fp, pc}
66	
67	@ length of .L._println_str0
68		.word 0
69	.L._println_str0:
70		.asciz ""
71	.align 4
72	_println:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		adr r0, .L._println_str0
78		bl puts
79		mov r0, #0
80		bl fflush
81		mov sp, fp
82		pop {fp, pc}
83	
84	_exit:
85		push {fp, lr}
86		mov fp, sp
87		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
88		bic sp, sp, #0x7
89		bl exit
90		mov sp, fp
91		pop {fp, pc}
92	
93	@ length of .L._errOverflow_str0
94		.word 52
95	.L._errOverflow_str0:
96		.asciz "fatal error: integer overflow or underflow occurred\n"
97	.align 4
98	_errOverflow:
99		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
100		bic sp, sp, #0x7
101		adr r0, .L._errOverflow_str0
102		bl _prints
103		mov r0, #255
104		bl _exit
===========================================================
-- Finished

