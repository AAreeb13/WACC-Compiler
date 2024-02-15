./valid/array/arraySimple.wacc
calling the reference compiler on ./valid/array/arraySimple.wacc
-- Test: arraySimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple array assignment and lookup

# Output:
# 42
#

# Program:

begin
  int[] a = [0] ;
  a[0] = 42 ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arraySimple.s contents are:
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
9		@ 1 element array
10		mov r0, #8
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #1
16		str r8, [r12, #-4]
17		mov r8, #0
18		str r8, [r12, #0]
19		mov r8, r12
20		mov r4, r8
21		mov r10, #0
22		mov r8, #42
23		mov r3, r4
24		bl _arrStore4
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r10, #0
27		mov r3, r4
28		bl _arrLoad4
29		mov r8, r3
30		mov r8, r8
31		mov r0, r8
32		@ statement primitives do not return results (but will clobber r0/rax)
33		bl _printi
34		bl _println
35		@ Stack pointer unchanged, no stack allocated variables
36		mov r0, #0
37		pop {r4, r8, r10, r12}
38		pop {fp, pc}
39	
40	@ length of .L._prints_str0
41		.word 4
42	.L._prints_str0:
43		.asciz "%.*s"
44	.align 4
45	_prints:
46		push {fp, lr}
47		mov fp, sp
48		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
49		bic sp, sp, #0x7
50		mov r2, r0
51		ldr r1, [r0, #-4]
52		adr r0, .L._prints_str0
53		bl printf
54		mov r0, #0
55		bl fflush
56		mov sp, fp
57		pop {fp, pc}
58	
59	_malloc:
60		push {fp, lr}
61		mov fp, sp
62		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
63		bic sp, sp, #0x7
64		bl malloc
65		cmp r0, #0
66		bleq _errOutOfMemory
67		mov sp, fp
68		pop {fp, pc}
69	
70	@ length of .L._printi_str0
71		.word 2
72	.L._printi_str0:
73		.asciz "%d"
74	.align 4
75	_printi:
76		push {fp, lr}
77		mov fp, sp
78		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
79		bic sp, sp, #0x7
80		mov r1, r0
81		adr r0, .L._printi_str0
82		bl printf
83		mov r0, #0
84		bl fflush
85		mov sp, fp
86		pop {fp, pc}
87	
88	@ length of .L._println_str0
89		.word 0
90	.L._println_str0:
91		.asciz ""
92	.align 4
93	_println:
94		push {fp, lr}
95		mov fp, sp
96		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
97		bic sp, sp, #0x7
98		adr r0, .L._println_str0
99		bl puts
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
114	_arrStore4:
115		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
116		push {lr}
117		cmp r10, #0
118		movlt r1, r10
119		bllt _errOutOfBounds
120		ldr lr, [r3, #-4]
121		cmp r10, lr
122		movge r1, r10
123		blge _errOutOfBounds
124		str r8, [r3, r10, lsl #2]
125		pop {pc}
126	
127	_arrLoad4:
128		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
129		push {lr}
130		cmp r10, #0
131		movlt r1, r10
132		bllt _errOutOfBounds
133		ldr lr, [r3, #-4]
134		cmp r10, lr
135		movge r1, r10
136		blge _errOutOfBounds
137		ldr r3, [r3, r10, lsl #2]
138		pop {pc}
139	
140	@ length of .L._errOutOfMemory_str0
141		.word 27
142	.L._errOutOfMemory_str0:
143		.asciz "fatal error: out of memory\n"
144	.align 4
145	_errOutOfMemory:
146		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
147		bic sp, sp, #0x7
148		adr r0, .L._errOutOfMemory_str0
149		bl _prints
150		mov r0, #255
151		bl _exit
152	
153	@ length of .L._errOutOfBounds_str0
154		.word 42
155	.L._errOutOfBounds_str0:
156		.asciz "fatal error: array index %d out of bounds\n"
157	.align 4
158	_errOutOfBounds:
159		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
160		bic sp, sp, #0x7
161		adr r0, .L._errOutOfBounds_str0
162		bl printf
163		mov r0, #0
164		bl fflush
165		mov r0, #255
166		bl _exit
===========================================================
-- Finished

