./valid/expressions/longSplitExpr.wacc
calling the reference compiler on ./valid/expressions/longSplitExpr.wacc
-- Test: longSplitExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions with several variables

# Output:

# Exit:
# 153

begin

  int a = 1 + 2 ;
  int b = 3 + 4 ;
  int c = 5 + 6 ;
  int d = 7 + 8 ;
  int e = 9 + 10 ;
  int f = 11 + 12 ;
  int g = 13 + 14 ;
  int h = 15 + 16 ;
  int i = 17 ;
  exit a + b + c + d + e + f + g + h + i

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longSplitExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r7, r8, r10, r12}
7		mov fp, sp
8		@ Stack padded to a multiple of the required alignment
9		sub sp, sp, #8
10		mov r8, #1
11		adds r8, r8, #2
12		blvs _errOverflow
13		push {r8}
14		pop {r8}
15		mov r8, r8
16		mov r4, r8
17		mov r8, #3
18		adds r8, r8, #4
19		blvs _errOverflow
20		push {r8}
21		pop {r8}
22		mov r8, r8
23		mov r5, r8
24		mov r8, #5
25		adds r8, r8, #6
26		blvs _errOverflow
27		push {r8}
28		pop {r8}
29		mov r8, r8
30		mov r6, r8
31		mov r8, #7
32		adds r8, r8, #8
33		blvs _errOverflow
34		push {r8}
35		pop {r8}
36		mov r8, r8
37		mov r7, r8
38		mov r8, #9
39		adds r8, r8, #10
40		blvs _errOverflow
41		push {r8}
42		pop {r8}
43		mov r8, r8
44		mov r0, r8
45		mov r8, #11
46		adds r8, r8, #12
47		blvs _errOverflow
48		push {r8}
49		pop {r8}
50		mov r8, r8
51		mov r1, r8
52		mov r8, #13
53		adds r8, r8, #14
54		blvs _errOverflow
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r2, r8
59		mov r8, #15
60		adds r8, r8, #16
61		blvs _errOverflow
62		push {r8}
63		pop {r8}
64		mov r8, r8
65		mov r3, r8
66		mov r8, #17
67		str r8, [fp, #-4]
68		push {r0, r1, r2, r3}
69		@ Set up R12 as a temporary second base pointer for the caller saved things
70		mov r12, sp
71		@ Stack pointer unchanged, no stack allocated arguments
72		adds r8, r4, r5
73		blvs _errOverflow
74		push {r8}
75		pop {r8}
76		adds r8, r8, r6
77		blvs _errOverflow
78		push {r8}
79		pop {r8}
80		adds r8, r8, r7
81		blvs _errOverflow
82		push {r8}
83		pop {r8}
84		adds r8, r8, r0
85		blvs _errOverflow
86		push {r8}
87		pop {r8}
88		adds r8, r8, r1
89		blvs _errOverflow
90		push {r8}
91		pop {r8}
92		adds r8, r8, r2
93		blvs _errOverflow
94		push {r8}
95		pop {r8}
96		adds r8, r8, r3
97		blvs _errOverflow
98		push {r8}
99		ldr r9, [fp, #-4]
100		pop {r8}
101		adds r8, r8, r9
102		blvs _errOverflow
103		push {r8}
104		pop {r8}
105		mov r8, r8
106		mov r0, r8
107		@ statement primitives do not return results (but will clobber r0/rax)
108		bl _exit
109		pop {r0, r1, r2, r3}
110		@ Stack padded to a multiple of the required alignment
111		add sp, sp, #8
112		mov r0, #0
113		pop {r4, r5, r6, r7, r8, r10, r12}
114		pop {fp, pc}
115	
116	@ length of .L._prints_str0
117		.word 4
118	.L._prints_str0:
119		.asciz "%.*s"
120	.align 4
121	_prints:
122		push {fp, lr}
123		mov fp, sp
124		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
125		bic sp, sp, #0x7
126		mov r2, r0
127		ldr r1, [r0, #-4]
128		adr r0, .L._prints_str0
129		bl printf
130		mov r0, #0
131		bl fflush
132		mov sp, fp
133		pop {fp, pc}
134	
135	_exit:
136		push {fp, lr}
137		mov fp, sp
138		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
139		bic sp, sp, #0x7
140		bl exit
141		mov sp, fp
142		pop {fp, pc}
143	
144	@ length of .L._errOverflow_str0
145		.word 52
146	.L._errOverflow_str0:
147		.asciz "fatal error: integer overflow or underflow occurred\n"
148	.align 4
149	_errOverflow:
150		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
151		bic sp, sp, #0x7
152		adr r0, .L._errOverflow_str0
153		bl _prints
154		mov r0, #255
155		bl _exit
===========================================================
-- Finished

