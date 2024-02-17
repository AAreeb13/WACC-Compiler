./valid/array/lenArrayIndex.wacc
calling the reference compiler on ./valid/array/lenArrayIndex.wacc
-- Test: lenArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Tests that array length works on array indexes
# Thanks to Nixon Enraght-Moony for his help in this test-case

# Output:
# 0

# Program:

begin
  int[] arr = [];
  int[][] arrs = [arr];
  print len arrs[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lenArrayIndex.s contents are:
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
9		@ 0 element array
10		mov r0, #4
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #0
16		str r8, [r12, #-4]
17		mov r8, r12
18		mov r4, r8
19		@ 1 element array
20		mov r0, #8
21		bl _malloc
22		mov r12, r0
23		@ array pointers are shifted forwards by 4 bytes (to account for size)
24		add r12, r12, #4
25		mov r8, #1
26		str r8, [r12, #-4]
27		mov r8, r4
28		str r8, [r12, #0]
29		mov r8, r12
30		mov r5, r8
31		@ Stack pointer unchanged, no stack allocated arguments
32		mov r10, #0
33		mov r3, r5
34		bl _arrLoad4
35		mov r8, r3
36		mov r8, r8
37		push {r8}
38		pop {r8}
39		ldr r8, [r8, #-4]
40		mov r8, r8
41		mov r0, r8
42		@ statement primitives do not return results (but will clobber r0/rax)
43		bl _printi
44		@ Stack pointer unchanged, no stack allocated variables
45		mov r0, #0
46		pop {r4, r5, r8, r10, r12}
47		pop {fp, pc}
48	
49	@ length of .L._prints_str0
50		.word 4
51	.L._prints_str0:
52		.asciz "%.*s"
53	.align 4
54	_prints:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		mov r2, r0
60		ldr r1, [r0, #-4]
61		adr r0, .L._prints_str0
62		bl printf
63		mov r0, #0
64		bl fflush
65		mov sp, fp
66		pop {fp, pc}
67	
68	_malloc:
69		push {fp, lr}
70		mov fp, sp
71		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
72		bic sp, sp, #0x7
73		bl malloc
74		cmp r0, #0
75		bleq _errOutOfMemory
76		mov sp, fp
77		pop {fp, pc}
78	
79	@ length of .L._printi_str0
80		.word 2
81	.L._printi_str0:
82		.asciz "%d"
83	.align 4
84	_printi:
85		push {fp, lr}
86		mov fp, sp
87		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
88		bic sp, sp, #0x7
89		mov r1, r0
90		adr r0, .L._printi_str0
91		bl printf
92		mov r0, #0
93		bl fflush
94		mov sp, fp
95		pop {fp, pc}
96	
97	_exit:
98		push {fp, lr}
99		mov fp, sp
100		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
101		bic sp, sp, #0x7
102		bl exit
103		mov sp, fp
104		pop {fp, pc}
105	
106	_arrLoad4:
107		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
108		push {lr}
109		cmp r10, #0
110		movlt r1, r10
111		bllt _errOutOfBounds
112		ldr lr, [r3, #-4]
113		cmp r10, lr
114		movge r1, r10
115		blge _errOutOfBounds
116		ldr r3, [r3, r10, lsl #2]
117		pop {pc}
118	
119	@ length of .L._errOutOfMemory_str0
120		.word 27
121	.L._errOutOfMemory_str0:
122		.asciz "fatal error: out of memory\n"
123	.align 4
124	_errOutOfMemory:
125		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
126		bic sp, sp, #0x7
127		adr r0, .L._errOutOfMemory_str0
128		bl _prints
129		mov r0, #255
130		bl _exit
131	
132	@ length of .L._errOutOfBounds_str0
133		.word 42
134	.L._errOutOfBounds_str0:
135		.asciz "fatal error: array index %d out of bounds\n"
136	.align 4
137	_errOutOfBounds:
138		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
139		bic sp, sp, #0x7
140		adr r0, .L._errOutOfBounds_str0
141		bl printf
142		mov r0, #0
143		bl fflush
144		mov r0, #255
145		bl _exit
===========================================================
-- Finished

