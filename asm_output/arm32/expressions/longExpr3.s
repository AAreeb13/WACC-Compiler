./valid/expressions/longExpr3.wacc
calling the reference compiler on ./valid/expressions/longExpr3.wacc
-- Test: longExpr3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 9

begin

  int x = ((((((((((((((((1 - 2) + 3) - 4) + 5) - 6) + 7) - 8) + 9) - 10) + 11) - 12) + 13) - 14) + 15) - 16) + 17);
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr3.s contents are:
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
9		mov r8, #1
10		subs r8, r8, #2
11		blvs _errOverflow
12		push {r8}
13		pop {r8}
14		adds r8, r8, #3
15		blvs _errOverflow
16		push {r8}
17		pop {r8}
18		subs r8, r8, #4
19		blvs _errOverflow
20		push {r8}
21		pop {r8}
22		adds r8, r8, #5
23		blvs _errOverflow
24		push {r8}
25		pop {r8}
26		subs r8, r8, #6
27		blvs _errOverflow
28		push {r8}
29		pop {r8}
30		adds r8, r8, #7
31		blvs _errOverflow
32		push {r8}
33		pop {r8}
34		subs r8, r8, #8
35		blvs _errOverflow
36		push {r8}
37		pop {r8}
38		adds r8, r8, #9
39		blvs _errOverflow
40		push {r8}
41		pop {r8}
42		subs r8, r8, #10
43		blvs _errOverflow
44		push {r8}
45		pop {r8}
46		adds r8, r8, #11
47		blvs _errOverflow
48		push {r8}
49		pop {r8}
50		subs r8, r8, #12
51		blvs _errOverflow
52		push {r8}
53		pop {r8}
54		adds r8, r8, #13
55		blvs _errOverflow
56		push {r8}
57		pop {r8}
58		subs r8, r8, #14
59		blvs _errOverflow
60		push {r8}
61		pop {r8}
62		adds r8, r8, #15
63		blvs _errOverflow
64		push {r8}
65		pop {r8}
66		subs r8, r8, #16
67		blvs _errOverflow
68		push {r8}
69		pop {r8}
70		adds r8, r8, #17
71		blvs _errOverflow
72		push {r8}
73		pop {r8}
74		mov r8, r8
75		mov r4, r8
76		@ Stack pointer unchanged, no stack allocated arguments
77		mov r8, r4
78		mov r0, r8
79		@ statement primitives do not return results (but will clobber r0/rax)
80		bl _exit
81		@ Stack pointer unchanged, no stack allocated variables
82		mov r0, #0
83		pop {r4, r8, r10, r12}
84		pop {fp, pc}
85	
86	@ length of .L._prints_str0
87		.word 4
88	.L._prints_str0:
89		.asciz "%.*s"
90	.align 4
91	_prints:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		mov r2, r0
97		ldr r1, [r0, #-4]
98		adr r0, .L._prints_str0
99		bl printf
100		mov r0, #0
101		bl fflush
102		mov sp, fp
103		pop {fp, pc}
104	
105	_exit:
106		push {fp, lr}
107		mov fp, sp
108		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
109		bic sp, sp, #0x7
110		bl exit
111		mov sp, fp
112		pop {fp, pc}
113	
114	@ length of .L._errOverflow_str0
115		.word 52
116	.L._errOverflow_str0:
117		.asciz "fatal error: integer overflow or underflow occurred\n"
118	.align 4
119	_errOverflow:
120		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
121		bic sp, sp, #0x7
122		adr r0, .L._errOverflow_str0
123		bl _prints
124		mov r0, #255
125		bl _exit
===========================================================
-- Finished

