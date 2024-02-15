./valid/expressions/longExpr2.wacc
calling the reference compiler on ./valid/expressions/longExpr2.wacc
-- Test: longExpr2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 10

begin

  int x = (2 + 3 + 2 + 1 + 1 + 1) - (1 + 2) * (3 - 4 / 6) / ( 2 * (18 - 17) + (3 * 4 / 4 + 6));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr2.s contents are:
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
9		mov r8, #2
10		adds r8, r8, #3
11		blvs _errOverflow
12		push {r8}
13		pop {r8}
14		adds r8, r8, #2
15		blvs _errOverflow
16		push {r8}
17		pop {r8}
18		adds r8, r8, #1
19		blvs _errOverflow
20		push {r8}
21		pop {r8}
22		adds r8, r8, #1
23		blvs _errOverflow
24		push {r8}
25		pop {r8}
26		adds r8, r8, #1
27		blvs _errOverflow
28		push {r8}
29		mov r8, #1
30		adds r8, r8, #2
31		blvs _errOverflow
32		push {r8}
33		mov r0, #4
34		mov r1, #6
35		cmp r1, #0
36		bleq _errDivZero
37		bl __aeabi_idivmod
38		mov r12, r0
39		mov r8, r12
40		push {r8}
41		pop {r9}
42		rsbs r8, r9, #3
43		blvs _errOverflow
44		push {r8}
45		pop {r9}
46		pop {r8}
47		smull r8, r9, r8, r9
48		@ Compare the sign of the lowest bits of the multiplication against the highest
49		cmp r9, r8, asr #31
50		blne _errOverflow
51		push {r8}
52		mov r8, #18
53		subs r8, r8, #17
54		blvs _errOverflow
55		push {r8}
56		pop {r9}
57		mov r8, #2
58		smull r8, r9, r8, r9
59		@ Compare the sign of the lowest bits of the multiplication against the highest
60		cmp r9, r8, asr #31
61		blne _errOverflow
62		push {r8}
63		mov r9, #4
64		mov r8, #3
65		smull r8, r9, r8, r9
66		@ Compare the sign of the lowest bits of the multiplication against the highest
67		cmp r9, r8, asr #31
68		blne _errOverflow
69		push {r8}
70		pop {r8}
71		mov r0, r8
72		mov r1, #4
73		cmp r1, #0
74		bleq _errDivZero
75		bl __aeabi_idivmod
76		mov r12, r0
77		mov r8, r12
78		push {r8}
79		pop {r8}
80		adds r8, r8, #6
81		blvs _errOverflow
82		push {r8}
83		pop {r9}
84		pop {r8}
85		adds r8, r8, r9
86		blvs _errOverflow
87		push {r8}
88		pop {r9}
89		pop {r8}
90		mov r0, r8
91		mov r1, r9
92		cmp r1, #0
93		bleq _errDivZero
94		bl __aeabi_idivmod
95		mov r12, r0
96		mov r8, r12
97		push {r8}
98		pop {r9}
99		pop {r8}
100		subs r8, r8, r9
101		blvs _errOverflow
102		push {r8}
103		pop {r8}
104		mov r8, r8
105		mov r4, r8
106		@ Stack pointer unchanged, no stack allocated arguments
107		mov r8, r4
108		mov r0, r8
109		@ statement primitives do not return results (but will clobber r0/rax)
110		bl _exit
111		@ Stack pointer unchanged, no stack allocated variables
112		mov r0, #0
113		pop {r4, r8, r10, r12}
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
144	@ length of .L._errDivZero_str0
145		.word 40
146	.L._errDivZero_str0:
147		.asciz "fatal error: division or modulo by zero\n"
148	.align 4
149	_errDivZero:
150		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
151		bic sp, sp, #0x7
152		adr r0, .L._errDivZero_str0
153		bl _prints
154		mov r0, #255
155		bl _exit
156	
157	@ length of .L._errOverflow_str0
158		.word 52
159	.L._errOverflow_str0:
160		.asciz "fatal error: integer overflow or underflow occurred\n"
161	.align 4
162	_errOverflow:
163		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
164		bic sp, sp, #0x7
165		adr r0, .L._errOverflow_str0
166		bl _prints
167		mov r0, #255
168		bl _exit
===========================================================
-- Finished

