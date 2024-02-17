./valid/expressions/andExpr.wacc
calling the reference compiler on ./valid/expressions/andExpr.wacc
-- Test: andExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating and

# Output:
# false
# true
# false
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println a && b ;
  println a && true ;
  println b && false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
andExpr.s contents are:
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
9		mov r8, #1
10		mov r4, r8
11		mov r8, #0
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		cmp r4, #1
15		bne .L0
16		cmp r5, #1
17	.L0:
18		moveq r8, #1
19		movne r8, #0
20		push {r8}
21		pop {r8}
22		mov r8, r8
23		mov r0, r8
24		@ statement primitives do not return results (but will clobber r0/rax)
25		bl _printb
26		bl _println
27		@ Stack pointer unchanged, no stack allocated arguments
28		cmp r4, #1
29		bne .L1
30		mov r8, #1
31		cmp r8, #1
32	.L1:
33		moveq r8, #1
34		movne r8, #0
35		push {r8}
36		pop {r8}
37		mov r8, r8
38		mov r0, r8
39		@ statement primitives do not return results (but will clobber r0/rax)
40		bl _printb
41		bl _println
42		@ Stack pointer unchanged, no stack allocated arguments
43		cmp r5, #1
44		bne .L2
45		mov r8, #0
46		cmp r8, #1
47	.L2:
48		moveq r8, #1
49		movne r8, #0
50		push {r8}
51		pop {r8}
52		mov r8, r8
53		mov r0, r8
54		@ statement primitives do not return results (but will clobber r0/rax)
55		bl _printb
56		bl _println
57		@ Stack pointer unchanged, no stack allocated variables
58		mov r0, #0
59		pop {r4, r5, r8, r10, r12}
60		pop {fp, pc}
61	
62	@ length of .L._printb_str0
63		.word 5
64	.L._printb_str0:
65		.asciz "false"
66	@ length of .L._printb_str1
67		.word 4
68	.L._printb_str1:
69		.asciz "true"
70	@ length of .L._printb_str2
71		.word 4
72	.L._printb_str2:
73		.asciz "%.*s"
74	.align 4
75	_printb:
76		push {fp, lr}
77		mov fp, sp
78		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
79		bic sp, sp, #0x7
80		cmp r0, #0
81		bne .L_printb0
82		adr r2, .L._printb_str0
83		b .L_printb1
84	.L_printb0:
85		adr r2, .L._printb_str1
86	.L_printb1:
87		ldr r1, [r2, #-4]
88		adr r0, .L._printb_str2
89		bl printf
90		mov r0, #0
91		bl fflush
92		mov sp, fp
93		pop {fp, pc}
94	
95	@ length of .L._println_str0
96		.word 0
97	.L._println_str0:
98		.asciz ""
99	.align 4
100	_println:
101		push {fp, lr}
102		mov fp, sp
103		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
104		bic sp, sp, #0x7
105		adr r0, .L._println_str0
106		bl puts
107		mov r0, #0
108		bl fflush
109		mov sp, fp
110		pop {fp, pc}
===========================================================
-- Finished

