./valid/expressions/charComparisonExpr.wacc
calling the reference compiler on ./valid/expressions/charComparisonExpr.wacc
-- Test: charComparisonExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# detailed battery of character comparison tests

# Output:
# false
# true
# true
# true
# false
# false
#

# Program:

begin
  char c1 = 'a' ;
  char c2 = 'z' ;
  println c1 == c2 ;
  println c1 != c2 ;
  println c1 < c2 ;
  println c1 <= c2 ;
  println c1 > c2 ;
  println c1 >= c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charComparisonExpr.s contents are:
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
9		mov r8, #97
10		mov r4, r8
11		mov r8, #122
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		cmp r4, r5
15		moveq r8, #1
16		movne r8, #0
17		push {r8}
18		pop {r8}
19		mov r8, r8
20		mov r0, r8
21		@ statement primitives do not return results (but will clobber r0/rax)
22		bl _printb
23		bl _println
24		@ Stack pointer unchanged, no stack allocated arguments
25		cmp r4, r5
26		movne r8, #1
27		moveq r8, #0
28		push {r8}
29		pop {r8}
30		mov r8, r8
31		mov r0, r8
32		@ statement primitives do not return results (but will clobber r0/rax)
33		bl _printb
34		bl _println
35		@ Stack pointer unchanged, no stack allocated arguments
36		cmp r4, r5
37		movlt r8, #1
38		movge r8, #0
39		push {r8}
40		pop {r8}
41		mov r8, r8
42		mov r0, r8
43		@ statement primitives do not return results (but will clobber r0/rax)
44		bl _printb
45		bl _println
46		@ Stack pointer unchanged, no stack allocated arguments
47		cmp r4, r5
48		movle r8, #1
49		movgt r8, #0
50		push {r8}
51		pop {r8}
52		mov r8, r8
53		mov r0, r8
54		@ statement primitives do not return results (but will clobber r0/rax)
55		bl _printb
56		bl _println
57		@ Stack pointer unchanged, no stack allocated arguments
58		cmp r4, r5
59		movgt r8, #1
60		movle r8, #0
61		push {r8}
62		pop {r8}
63		mov r8, r8
64		mov r0, r8
65		@ statement primitives do not return results (but will clobber r0/rax)
66		bl _printb
67		bl _println
68		@ Stack pointer unchanged, no stack allocated arguments
69		cmp r4, r5
70		movge r8, #1
71		movlt r8, #0
72		push {r8}
73		pop {r8}
74		mov r8, r8
75		mov r0, r8
76		@ statement primitives do not return results (but will clobber r0/rax)
77		bl _printb
78		bl _println
79		@ Stack pointer unchanged, no stack allocated variables
80		mov r0, #0
81		pop {r4, r5, r8, r10, r12}
82		pop {fp, pc}
83	
84	@ length of .L._printb_str0
85		.word 5
86	.L._printb_str0:
87		.asciz "false"
88	@ length of .L._printb_str1
89		.word 4
90	.L._printb_str1:
91		.asciz "true"
92	@ length of .L._printb_str2
93		.word 4
94	.L._printb_str2:
95		.asciz "%.*s"
96	.align 4
97	_printb:
98		push {fp, lr}
99		mov fp, sp
100		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
101		bic sp, sp, #0x7
102		cmp r0, #0
103		bne .L_printb0
104		adr r2, .L._printb_str0
105		b .L_printb1
106	.L_printb0:
107		adr r2, .L._printb_str1
108	.L_printb1:
109		ldr r1, [r2, #-4]
110		adr r0, .L._printb_str2
111		bl printf
112		mov r0, #0
113		bl fflush
114		mov sp, fp
115		pop {fp, pc}
116	
117	@ length of .L._println_str0
118		.word 0
119	.L._println_str0:
120		.asciz ""
121	.align 4
122	_println:
123		push {fp, lr}
124		mov fp, sp
125		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
126		bic sp, sp, #0x7
127		adr r0, .L._println_str0
128		bl puts
129		mov r0, #0
130		bl fflush
131		mov sp, fp
132		pop {fp, pc}
===========================================================
-- Finished

