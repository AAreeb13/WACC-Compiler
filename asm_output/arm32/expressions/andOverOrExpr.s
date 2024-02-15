./valid/expressions/andOverOrExpr.wacc
calling the reference compiler on ./valid/expressions/andOverOrExpr.wacc
-- Test: andOverOrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool a = false ;
  bool b = false ;
  bool c = true ;
  println a && b || c ;
  println a && (b || c)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
andOverOrExpr.s contents are:
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
9		mov r8, #0
10		mov r4, r8
11		mov r8, #0
12		mov r5, r8
13		mov r8, #1
14		mov r6, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		cmp r4, #1
17		bne .L0
18		cmp r5, #1
19	.L0:
20		moveq r8, #1
21		movne r8, #0
22		push {r8}
23		pop {r8}
24		cmp r8, #1
25		beq .L1
26		cmp r6, #1
27	.L1:
28		moveq r8, #1
29		movne r8, #0
30		push {r8}
31		pop {r8}
32		mov r8, r8
33		mov r0, r8
34		@ statement primitives do not return results (but will clobber r0/rax)
35		bl _printb
36		bl _println
37		@ Stack pointer unchanged, no stack allocated arguments
38		cmp r4, #1
39		bne .L2
40		cmp r5, #1
41		beq .L3
42		cmp r6, #1
43	.L3:
44		moveq r8, #1
45		movne r8, #0
46		push {r8}
47		pop {r8}
48		cmp r8, #1
49	.L2:
50		moveq r8, #1
51		movne r8, #0
52		push {r8}
53		pop {r8}
54		mov r8, r8
55		mov r0, r8
56		@ statement primitives do not return results (but will clobber r0/rax)
57		bl _printb
58		bl _println
59		@ Stack pointer unchanged, no stack allocated variables
60		mov r0, #0
61		pop {r4, r5, r6, r8, r10, r12}
62		pop {fp, pc}
63	
64	@ length of .L._printb_str0
65		.word 5
66	.L._printb_str0:
67		.asciz "false"
68	@ length of .L._printb_str1
69		.word 4
70	.L._printb_str1:
71		.asciz "true"
72	@ length of .L._printb_str2
73		.word 4
74	.L._printb_str2:
75		.asciz "%.*s"
76	.align 4
77	_printb:
78		push {fp, lr}
79		mov fp, sp
80		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
81		bic sp, sp, #0x7
82		cmp r0, #0
83		bne .L_printb0
84		adr r2, .L._printb_str0
85		b .L_printb1
86	.L_printb0:
87		adr r2, .L._printb_str1
88	.L_printb1:
89		ldr r1, [r2, #-4]
90		adr r0, .L._printb_str2
91		bl printf
92		mov r0, #0
93		bl fflush
94		mov sp, fp
95		pop {fp, pc}
96	
97	@ length of .L._println_str0
98		.word 0
99	.L._println_str0:
100		.asciz ""
101	.align 4
102	_println:
103		push {fp, lr}
104		mov fp, sp
105		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
106		bic sp, sp, #0x7
107		adr r0, .L._println_str0
108		bl puts
109		mov r0, #0
110		bl fflush
111		mov sp, fp
112		pop {fp, pc}
===========================================================
-- Finished

