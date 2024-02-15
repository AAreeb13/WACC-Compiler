./valid/expressions/longExpr.wacc
calling the reference compiler on ./valid/expressions/longExpr.wacc
-- Test: longExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 153

begin

  int x = 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + (10 + (11 + (12 + (13 + (14 + (15 + (16 + 17)))))))))))))));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #16
10		adds r8, r8, #17
11		blvs _errOverflow
12		push {r8}
13		pop {r9}
14		mov r8, #15
15		adds r8, r8, r9
16		blvs _errOverflow
17		push {r8}
18		pop {r9}
19		mov r8, #14
20		adds r8, r8, r9
21		blvs _errOverflow
22		push {r8}
23		pop {r9}
24		mov r8, #13
25		adds r8, r8, r9
26		blvs _errOverflow
27		push {r8}
28		pop {r9}
29		mov r8, #12
30		adds r8, r8, r9
31		blvs _errOverflow
32		push {r8}
33		pop {r9}
34		mov r8, #11
35		adds r8, r8, r9
36		blvs _errOverflow
37		push {r8}
38		pop {r9}
39		mov r8, #10
40		adds r8, r8, r9
41		blvs _errOverflow
42		push {r8}
43		pop {r9}
44		mov r8, #9
45		adds r8, r8, r9
46		blvs _errOverflow
47		push {r8}
48		pop {r9}
49		mov r8, #8
50		adds r8, r8, r9
51		blvs _errOverflow
52		push {r8}
53		pop {r9}
54		mov r8, #7
55		adds r8, r8, r9
56		blvs _errOverflow
57		push {r8}
58		pop {r9}
59		mov r8, #6
60		adds r8, r8, r9
61		blvs _errOverflow
62		push {r8}
63		pop {r9}
64		mov r8, #5
65		adds r8, r8, r9
66		blvs _errOverflow
67		push {r8}
68		pop {r9}
69		mov r8, #4
70		adds r8, r8, r9
71		blvs _errOverflow
72		push {r8}
73		pop {r9}
74		mov r8, #3
75		adds r8, r8, r9
76		blvs _errOverflow
77		push {r8}
78		pop {r9}
79		mov r8, #2
80		adds r8, r8, r9
81		blvs _errOverflow
82		push {r8}
83		pop {r9}
84		mov r8, #1
85		adds r8, r8, r9
86		blvs _errOverflow
87		push {r8}
88		pop {r8}
89		mov r8, r8
90		mov r4, r8
91		@ Stack pointer unchanged, no stack allocated arguments
92		mov r8, r4
93		mov r0, r8
94		@ statement primitives do not return results (but will clobber r0/rax)
95		bl _exit
96		@ Stack pointer unchanged, no stack allocated variables
97		mov r0, #0
98		pop {r4, r8, r10, r12}
99		pop {fp, pc}
100	
101	@ length of .L._prints_str0
102		.word 4
103	.L._prints_str0:
104		.asciz "%.*s"
105	.align 4
106	_prints:
107		push {fp, lr}
108		mov fp, sp
109		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
110		bic sp, sp, #0x7
111		mov r2, r0
112		ldr r1, [r0, #-4]
113		adr r0, .L._prints_str0
114		bl printf
115		mov r0, #0
116		bl fflush
117		mov sp, fp
118		pop {fp, pc}
119	
120	_exit:
121		push {fp, lr}
122		mov fp, sp
123		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
124		bic sp, sp, #0x7
125		bl exit
126		mov sp, fp
127		pop {fp, pc}
128	
129	@ length of .L._errOverflow_str0
130		.word 52
131	.L._errOverflow_str0:
132		.asciz "fatal error: integer overflow or underflow occurred\n"
133	.align 4
134	_errOverflow:
135		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
136		bic sp, sp, #0x7
137		adr r0, .L._errOverflow_str0
138		bl _prints
139		mov r0, #255
140		bl _exit
===========================================================
-- Finished

