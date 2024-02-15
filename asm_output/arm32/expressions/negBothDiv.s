./valid/expressions/negBothDiv.wacc
calling the reference compiler on ./valid/expressions/negBothDiv.wacc
-- Test: negBothDiv.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division of a negative number by a negative number

# Output:
# 2
#

# Program:

begin
  int x = -4 ;
  int y = -2 ;
  println x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negBothDiv.s contents are:
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
9		ldr r8, =-4
10		mov r8, r8
11		mov r4, r8
12		ldr r8, =-2
13		mov r8, r8
14		mov r5, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		mov r0, r4
17		mov r1, r5
18		cmp r1, #0
19		bleq _errDivZero
20		bl __aeabi_idivmod
21		mov r12, r0
22		mov r8, r12
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _printi
29		bl _println
30		@ Stack pointer unchanged, no stack allocated variables
31		mov r0, #0
32		pop {r4, r5, r8, r10, r12}
33		pop {fp, pc}
34	
35	@ length of .L._prints_str0
36		.word 4
37	.L._prints_str0:
38		.asciz "%.*s"
39	.align 4
40	_prints:
41		push {fp, lr}
42		mov fp, sp
43		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
44		bic sp, sp, #0x7
45		mov r2, r0
46		ldr r1, [r0, #-4]
47		adr r0, .L._prints_str0
48		bl printf
49		mov r0, #0
50		bl fflush
51		mov sp, fp
52		pop {fp, pc}
53	
54	@ length of .L._printi_str0
55		.word 2
56	.L._printi_str0:
57		.asciz "%d"
58	.align 4
59	_printi:
60		push {fp, lr}
61		mov fp, sp
62		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
63		bic sp, sp, #0x7
64		mov r1, r0
65		adr r0, .L._printi_str0
66		bl printf
67		mov r0, #0
68		bl fflush
69		mov sp, fp
70		pop {fp, pc}
71	
72	@ length of .L._println_str0
73		.word 0
74	.L._println_str0:
75		.asciz ""
76	.align 4
77	_println:
78		push {fp, lr}
79		mov fp, sp
80		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
81		bic sp, sp, #0x7
82		adr r0, .L._println_str0
83		bl puts
84		mov r0, #0
85		bl fflush
86		mov sp, fp
87		pop {fp, pc}
88	
89	_exit:
90		push {fp, lr}
91		mov fp, sp
92		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
93		bic sp, sp, #0x7
94		bl exit
95		mov sp, fp
96		pop {fp, pc}
97	
98	@ length of .L._errDivZero_str0
99		.word 40
100	.L._errDivZero_str0:
101		.asciz "fatal error: division or modulo by zero\n"
102	.align 4
103	_errDivZero:
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		adr r0, .L._errDivZero_str0
107		bl _prints
108		mov r0, #255
109		bl _exit
===========================================================
-- Finished

