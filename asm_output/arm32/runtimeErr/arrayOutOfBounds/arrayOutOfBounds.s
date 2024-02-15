./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc
-- Test: arrayOutOfBounds.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt out of bounds array access (this ought to seg fault or similar)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBounds.s contents are:
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
9		@ 3 element array
10		mov r0, #16
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #3
16		str r8, [r12, #-4]
17		mov r8, #1
18		str r8, [r12, #0]
19		mov r8, #2
20		str r8, [r12, #4]
21		mov r8, #3
22		str r8, [r12, #8]
23		mov r8, r12
24		mov r4, r8
25		@ 4 element array
26		mov r0, #20
27		bl _malloc
28		mov r12, r0
29		@ array pointers are shifted forwards by 4 bytes (to account for size)
30		add r12, r12, #4
31		mov r8, #4
32		str r8, [r12, #-4]
33		mov r8, #43
34		str r8, [r12, #0]
35		mov r8, #2
36		str r8, [r12, #4]
37		mov r8, #18
38		str r8, [r12, #8]
39		mov r8, #1
40		str r8, [r12, #12]
41		mov r8, r12
42		mov r5, r8
43		@ Stack pointer unchanged, no stack allocated arguments
44		mov r10, #5
45		mov r3, r5
46		bl _arrLoad4
47		mov r8, r3
48		mov r8, r8
49		mov r0, r8
50		@ statement primitives do not return results (but will clobber r0/rax)
51		bl _printi
52		bl _println
53		@ Stack pointer unchanged, no stack allocated variables
54		mov r0, #0
55		pop {r4, r5, r8, r10, r12}
56		pop {fp, pc}
57	
58	@ length of .L._prints_str0
59		.word 4
60	.L._prints_str0:
61		.asciz "%.*s"
62	.align 4
63	_prints:
64		push {fp, lr}
65		mov fp, sp
66		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
67		bic sp, sp, #0x7
68		mov r2, r0
69		ldr r1, [r0, #-4]
70		adr r0, .L._prints_str0
71		bl printf
72		mov r0, #0
73		bl fflush
74		mov sp, fp
75		pop {fp, pc}
76	
77	_malloc:
78		push {fp, lr}
79		mov fp, sp
80		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
81		bic sp, sp, #0x7
82		bl malloc
83		cmp r0, #0
84		bleq _errOutOfMemory
85		mov sp, fp
86		pop {fp, pc}
87	
88	@ length of .L._printi_str0
89		.word 2
90	.L._printi_str0:
91		.asciz "%d"
92	.align 4
93	_printi:
94		push {fp, lr}
95		mov fp, sp
96		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
97		bic sp, sp, #0x7
98		mov r1, r0
99		adr r0, .L._printi_str0
100		bl printf
101		mov r0, #0
102		bl fflush
103		mov sp, fp
104		pop {fp, pc}
105	
106	@ length of .L._println_str0
107		.word 0
108	.L._println_str0:
109		.asciz ""
110	.align 4
111	_println:
112		push {fp, lr}
113		mov fp, sp
114		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
115		bic sp, sp, #0x7
116		adr r0, .L._println_str0
117		bl puts
118		mov r0, #0
119		bl fflush
120		mov sp, fp
121		pop {fp, pc}
122	
123	_exit:
124		push {fp, lr}
125		mov fp, sp
126		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
127		bic sp, sp, #0x7
128		bl exit
129		mov sp, fp
130		pop {fp, pc}
131	
132	_arrLoad4:
133		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
134		push {lr}
135		cmp r10, #0
136		movlt r1, r10
137		bllt _errOutOfBounds
138		ldr lr, [r3, #-4]
139		cmp r10, lr
140		movge r1, r10
141		blge _errOutOfBounds
142		ldr r3, [r3, r10, lsl #2]
143		pop {pc}
144	
145	@ length of .L._errOutOfMemory_str0
146		.word 27
147	.L._errOutOfMemory_str0:
148		.asciz "fatal error: out of memory\n"
149	.align 4
150	_errOutOfMemory:
151		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
152		bic sp, sp, #0x7
153		adr r0, .L._errOutOfMemory_str0
154		bl _prints
155		mov r0, #255
156		bl _exit
157	
158	@ length of .L._errOutOfBounds_str0
159		.word 42
160	.L._errOutOfBounds_str0:
161		.asciz "fatal error: array index %d out of bounds\n"
162	.align 4
163	_errOutOfBounds:
164		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
165		bic sp, sp, #0x7
166		adr r0, .L._errOutOfBounds_str0
167		bl printf
168		mov r0, #0
169		bl fflush
170		mov r0, #255
171		bl _exit
===========================================================
-- Finished

