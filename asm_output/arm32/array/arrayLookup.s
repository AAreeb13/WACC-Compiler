./valid/array/arrayLookup.wacc
calling the reference compiler on ./valid/array/arrayLookup.wacc
-- Test: arrayLookup.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check first element of array

# Output:
# 43
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLookup.s contents are:
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
27		@ Stack pointer unchanged, no stack allocated arguments
28		mov r10, #0
29		mov r3, r4
30		bl _arrLoad4
31		mov r8, r3
32		mov r8, r8
33		mov r0, r8
34		@ statement primitives do not return results (but will clobber r0/rax)
35		bl _printi
36		bl _println
37		@ Stack pointer unchanged, no stack allocated variables
38		mov r0, #0
39		pop {r4, r8, r10, r12}
40		pop {fp, pc}
41	
42	@ length of .L._prints_str0
43		.word 4
44	.L._prints_str0:
45		.asciz "%.*s"
46	.align 4
47	_prints:
48		push {fp, lr}
49		mov fp, sp
50		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
51		bic sp, sp, #0x7
52		mov r2, r0
53		ldr r1, [r0, #-4]
54		adr r0, .L._prints_str0
55		bl printf
56		mov r0, #0
57		bl fflush
58		mov sp, fp
59		pop {fp, pc}
60	
61	_malloc:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		bl malloc
67		cmp r0, #0
68		bleq _errOutOfMemory
69		mov sp, fp
70		pop {fp, pc}
71	
72	@ length of .L._printi_str0
73		.word 2
74	.L._printi_str0:
75		.asciz "%d"
76	.align 4
77	_printi:
78		push {fp, lr}
79		mov fp, sp
80		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
81		bic sp, sp, #0x7
82		mov r1, r0
83		adr r0, .L._printi_str0
84		bl printf
85		mov r0, #0
86		bl fflush
87		mov sp, fp
88		pop {fp, pc}
89	
90	@ length of .L._println_str0
91		.word 0
92	.L._println_str0:
93		.asciz ""
94	.align 4
95	_println:
96		push {fp, lr}
97		mov fp, sp
98		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
99		bic sp, sp, #0x7
100		adr r0, .L._println_str0
101		bl puts
102		mov r0, #0
103		bl fflush
104		mov sp, fp
105		pop {fp, pc}
106	
107	_exit:
108		push {fp, lr}
109		mov fp, sp
110		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
111		bic sp, sp, #0x7
112		bl exit
113		mov sp, fp
114		pop {fp, pc}
115	
116	_arrLoad4:
117		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
118		push {lr}
119		cmp r10, #0
120		movlt r1, r10
121		bllt _errOutOfBounds
122		ldr lr, [r3, #-4]
123		cmp r10, lr
124		movge r1, r10
125		blge _errOutOfBounds
126		ldr r3, [r3, r10, lsl #2]
127		pop {pc}
128	
129	@ length of .L._errOutOfMemory_str0
130		.word 27
131	.L._errOutOfMemory_str0:
132		.asciz "fatal error: out of memory\n"
133	.align 4
134	_errOutOfMemory:
135		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
136		bic sp, sp, #0x7
137		adr r0, .L._errOutOfMemory_str0
138		bl _prints
139		mov r0, #255
140		bl _exit
141	
142	@ length of .L._errOutOfBounds_str0
143		.word 42
144	.L._errOutOfBounds_str0:
145		.asciz "fatal error: array index %d out of bounds\n"
146	.align 4
147	_errOutOfBounds:
148		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
149		bic sp, sp, #0x7
150		adr r0, .L._errOutOfBounds_str0
151		bl printf
152		mov r0, #0
153		bl fflush
154		mov r0, #255
155		bl _exit
===========================================================
-- Finished

