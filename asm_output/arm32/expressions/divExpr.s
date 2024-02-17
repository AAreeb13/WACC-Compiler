./valid/expressions/divExpr.wacc
calling the reference compiler on ./valid/expressions/divExpr.wacc
-- Test: divExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating integer division

# Output:
# 1
#

# Program:

begin
  int x = 5 ;
  int y = 3 ;
  println x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divExpr.s contents are:
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
9		mov r8, #5
10		mov r4, r8
11		mov r8, #3
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		mov r0, r4
15		mov r1, r5
16		cmp r1, #0
17		bleq _errDivZero
18		bl __aeabi_idivmod
19		mov r12, r0
20		mov r8, r12
21		push {r8}
22		pop {r8}
23		mov r8, r8
24		mov r0, r8
25		@ statement primitives do not return results (but will clobber r0/rax)
26		bl _printi
27		bl _println
28		@ Stack pointer unchanged, no stack allocated variables
29		mov r0, #0
30		pop {r4, r5, r8, r10, r12}
31		pop {fp, pc}
32	
33	@ length of .L._prints_str0
34		.word 4
35	.L._prints_str0:
36		.asciz "%.*s"
37	.align 4
38	_prints:
39		push {fp, lr}
40		mov fp, sp
41		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
42		bic sp, sp, #0x7
43		mov r2, r0
44		ldr r1, [r0, #-4]
45		adr r0, .L._prints_str0
46		bl printf
47		mov r0, #0
48		bl fflush
49		mov sp, fp
50		pop {fp, pc}
51	
52	@ length of .L._printi_str0
53		.word 2
54	.L._printi_str0:
55		.asciz "%d"
56	.align 4
57	_printi:
58		push {fp, lr}
59		mov fp, sp
60		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
61		bic sp, sp, #0x7
62		mov r1, r0
63		adr r0, .L._printi_str0
64		bl printf
65		mov r0, #0
66		bl fflush
67		mov sp, fp
68		pop {fp, pc}
69	
70	@ length of .L._println_str0
71		.word 0
72	.L._println_str0:
73		.asciz ""
74	.align 4
75	_println:
76		push {fp, lr}
77		mov fp, sp
78		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
79		bic sp, sp, #0x7
80		adr r0, .L._println_str0
81		bl puts
82		mov r0, #0
83		bl fflush
84		mov sp, fp
85		pop {fp, pc}
86	
87	_exit:
88		push {fp, lr}
89		mov fp, sp
90		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
91		bic sp, sp, #0x7
92		bl exit
93		mov sp, fp
94		pop {fp, pc}
95	
96	@ length of .L._errDivZero_str0
97		.word 40
98	.L._errDivZero_str0:
99		.asciz "fatal error: division or modulo by zero\n"
100	.align 4
101	_errDivZero:
102		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
103		bic sp, sp, #0x7
104		adr r0, .L._errDivZero_str0
105		bl _prints
106		mov r0, #255
107		bl _exit
===========================================================
-- Finished

