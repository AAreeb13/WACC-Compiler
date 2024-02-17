./valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc
-- Test: arrayNegBounds.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt out of bounds array access (this ought to seg fault or similar)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	int[] a = [43, 2, 18, 1] ;
  int[] b = [1, 2, 3] ;	
  println a[-2]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayNegBounds.s contents are:
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
9		@ 4 element array
10		mov r0, #20
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #4
16		str r8, [r12, #-4]
17		mov r8, #43
18		str r8, [r12, #0]
19		mov r8, #2
20		str r8, [r12, #4]
21		mov r8, #18
22		str r8, [r12, #8]
23		mov r8, #1
24		str r8, [r12, #12]
25		mov r8, r12
26		mov r4, r8
27		@ 3 element array
28		mov r0, #16
29		bl _malloc
30		mov r12, r0
31		@ array pointers are shifted forwards by 4 bytes (to account for size)
32		add r12, r12, #4
33		mov r8, #3
34		str r8, [r12, #-4]
35		mov r8, #1
36		str r8, [r12, #0]
37		mov r8, #2
38		str r8, [r12, #4]
39		mov r8, #3
40		str r8, [r12, #8]
41		mov r8, r12
42		mov r5, r8
43		@ Stack pointer unchanged, no stack allocated arguments
44		ldr r10, =-2
45		mov r10, r10
46		mov r3, r4
47		bl _arrLoad4
48		mov r8, r3
49		mov r8, r8
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _printi
53		bl _println
54		@ Stack pointer unchanged, no stack allocated variables
55		mov r0, #0
56		pop {r4, r5, r8, r10, r12}
57		pop {fp, pc}
58	
59	@ length of .L._prints_str0
60		.word 4
61	.L._prints_str0:
62		.asciz "%.*s"
63	.align 4
64	_prints:
65		push {fp, lr}
66		mov fp, sp
67		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
68		bic sp, sp, #0x7
69		mov r2, r0
70		ldr r1, [r0, #-4]
71		adr r0, .L._prints_str0
72		bl printf
73		mov r0, #0
74		bl fflush
75		mov sp, fp
76		pop {fp, pc}
77	
78	_malloc:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		bl malloc
84		cmp r0, #0
85		bleq _errOutOfMemory
86		mov sp, fp
87		pop {fp, pc}
88	
89	@ length of .L._printi_str0
90		.word 2
91	.L._printi_str0:
92		.asciz "%d"
93	.align 4
94	_printi:
95		push {fp, lr}
96		mov fp, sp
97		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
98		bic sp, sp, #0x7
99		mov r1, r0
100		adr r0, .L._printi_str0
101		bl printf
102		mov r0, #0
103		bl fflush
104		mov sp, fp
105		pop {fp, pc}
106	
107	@ length of .L._println_str0
108		.word 0
109	.L._println_str0:
110		.asciz ""
111	.align 4
112	_println:
113		push {fp, lr}
114		mov fp, sp
115		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
116		bic sp, sp, #0x7
117		adr r0, .L._println_str0
118		bl puts
119		mov r0, #0
120		bl fflush
121		mov sp, fp
122		pop {fp, pc}
123	
124	_exit:
125		push {fp, lr}
126		mov fp, sp
127		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
128		bic sp, sp, #0x7
129		bl exit
130		mov sp, fp
131		pop {fp, pc}
132	
133	_arrLoad4:
134		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
135		push {lr}
136		cmp r10, #0
137		movlt r1, r10
138		bllt _errOutOfBounds
139		ldr lr, [r3, #-4]
140		cmp r10, lr
141		movge r1, r10
142		blge _errOutOfBounds
143		ldr r3, [r3, r10, lsl #2]
144		pop {pc}
145	
146	@ length of .L._errOutOfMemory_str0
147		.word 27
148	.L._errOutOfMemory_str0:
149		.asciz "fatal error: out of memory\n"
150	.align 4
151	_errOutOfMemory:
152		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
153		bic sp, sp, #0x7
154		adr r0, .L._errOutOfMemory_str0
155		bl _prints
156		mov r0, #255
157		bl _exit
158	
159	@ length of .L._errOutOfBounds_str0
160		.word 42
161	.L._errOutOfBounds_str0:
162		.asciz "fatal error: array index %d out of bounds\n"
163	.align 4
164	_errOutOfBounds:
165		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
166		bic sp, sp, #0x7
167		adr r0, .L._errOutOfBounds_str0
168		bl printf
169		mov r0, #0
170		bl fflush
171		mov r0, #255
172		bl _exit
===========================================================
-- Finished

