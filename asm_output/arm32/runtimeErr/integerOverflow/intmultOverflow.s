./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
-- Test: intmultOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer overflow - generates odd assembly error!

# Output:
# 2147483
# 2147483000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2147483 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ; #err here?
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intmultOverflow.s contents are:
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
9		ldr r8, =2147483
10		mov r8, r8
11		mov r4, r8
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, r4
14		mov r0, r8
15		@ statement primitives do not return results (but will clobber r0/rax)
16		bl _printi
17		bl _println
18		mov r9, #1000
19		smull r8, r9, r4, r9
20		@ Compare the sign of the lowest bits of the multiplication against the highest
21		cmp r9, r8, asr #31
22		blne _errOverflow
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated arguments
28		mov r8, r4
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _printi
32		bl _println
33		mov r9, #1000
34		smull r8, r9, r4, r9
35		@ Compare the sign of the lowest bits of the multiplication against the highest
36		cmp r9, r8, asr #31
37		blne _errOverflow
38		push {r8}
39		pop {r8}
40		mov r8, r8
41		mov r4, r8
42		@ Stack pointer unchanged, no stack allocated arguments
43		mov r8, r4
44		mov r0, r8
45		@ statement primitives do not return results (but will clobber r0/rax)
46		bl _printi
47		bl _println
48		mov r9, #1000
49		smull r8, r9, r4, r9
50		@ Compare the sign of the lowest bits of the multiplication against the highest
51		cmp r9, r8, asr #31
52		blne _errOverflow
53		push {r8}
54		pop {r8}
55		mov r8, r8
56		mov r4, r8
57		@ Stack pointer unchanged, no stack allocated arguments
58		mov r8, r4
59		mov r0, r8
60		@ statement primitives do not return results (but will clobber r0/rax)
61		bl _printi
62		bl _println
63		@ Stack pointer unchanged, no stack allocated variables
64		mov r0, #0
65		pop {r4, r8, r10, r12}
66		pop {fp, pc}
67	
68	@ length of .L._prints_str0
69		.word 4
70	.L._prints_str0:
71		.asciz "%.*s"
72	.align 4
73	_prints:
74		push {fp, lr}
75		mov fp, sp
76		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
77		bic sp, sp, #0x7
78		mov r2, r0
79		ldr r1, [r0, #-4]
80		adr r0, .L._prints_str0
81		bl printf
82		mov r0, #0
83		bl fflush
84		mov sp, fp
85		pop {fp, pc}
86	
87	@ length of .L._printi_str0
88		.word 2
89	.L._printi_str0:
90		.asciz "%d"
91	.align 4
92	_printi:
93		push {fp, lr}
94		mov fp, sp
95		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
96		bic sp, sp, #0x7
97		mov r1, r0
98		adr r0, .L._printi_str0
99		bl printf
100		mov r0, #0
101		bl fflush
102		mov sp, fp
103		pop {fp, pc}
104	
105	@ length of .L._println_str0
106		.word 0
107	.L._println_str0:
108		.asciz ""
109	.align 4
110	_println:
111		push {fp, lr}
112		mov fp, sp
113		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
114		bic sp, sp, #0x7
115		adr r0, .L._println_str0
116		bl puts
117		mov r0, #0
118		bl fflush
119		mov sp, fp
120		pop {fp, pc}
121	
122	_exit:
123		push {fp, lr}
124		mov fp, sp
125		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
126		bic sp, sp, #0x7
127		bl exit
128		mov sp, fp
129		pop {fp, pc}
130	
131	@ length of .L._errOverflow_str0
132		.word 52
133	.L._errOverflow_str0:
134		.asciz "fatal error: integer overflow or underflow occurred\n"
135	.align 4
136	_errOverflow:
137		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
138		bic sp, sp, #0x7
139		adr r0, .L._errOverflow_str0
140		bl _prints
141		mov r0, #255
142		bl _exit
===========================================================
-- Finished

