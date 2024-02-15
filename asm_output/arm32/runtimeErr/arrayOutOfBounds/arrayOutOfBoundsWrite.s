./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
-- Test: arrayOutOfBoundsWrite.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt write out of array bounds (gods, this really should not work!)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	a[5] = 100 ;
  println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBoundsWrite.s contents are:
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
43		mov r10, #5
44		mov r8, #100
45		mov r3, r5
46		bl _arrStore4
47		@ Stack pointer unchanged, no stack allocated arguments
48		mov r10, #5
49		mov r3, r5
50		bl _arrLoad4
51		mov r8, r3
52		mov r8, r8
53		mov r0, r8
54		@ statement primitives do not return results (but will clobber r0/rax)
55		bl _printi
56		bl _println
57		@ Stack pointer unchanged, no stack allocated variables
58		mov r0, #0
59		pop {r4, r5, r8, r10, r12}
60		pop {fp, pc}
61	
62	@ length of .L._prints_str0
63		.word 4
64	.L._prints_str0:
65		.asciz "%.*s"
66	.align 4
67	_prints:
68		push {fp, lr}
69		mov fp, sp
70		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
71		bic sp, sp, #0x7
72		mov r2, r0
73		ldr r1, [r0, #-4]
74		adr r0, .L._prints_str0
75		bl printf
76		mov r0, #0
77		bl fflush
78		mov sp, fp
79		pop {fp, pc}
80	
81	_malloc:
82		push {fp, lr}
83		mov fp, sp
84		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
85		bic sp, sp, #0x7
86		bl malloc
87		cmp r0, #0
88		bleq _errOutOfMemory
89		mov sp, fp
90		pop {fp, pc}
91	
92	@ length of .L._printi_str0
93		.word 2
94	.L._printi_str0:
95		.asciz "%d"
96	.align 4
97	_printi:
98		push {fp, lr}
99		mov fp, sp
100		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
101		bic sp, sp, #0x7
102		mov r1, r0
103		adr r0, .L._printi_str0
104		bl printf
105		mov r0, #0
106		bl fflush
107		mov sp, fp
108		pop {fp, pc}
109	
110	@ length of .L._println_str0
111		.word 0
112	.L._println_str0:
113		.asciz ""
114	.align 4
115	_println:
116		push {fp, lr}
117		mov fp, sp
118		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
119		bic sp, sp, #0x7
120		adr r0, .L._println_str0
121		bl puts
122		mov r0, #0
123		bl fflush
124		mov sp, fp
125		pop {fp, pc}
126	
127	_exit:
128		push {fp, lr}
129		mov fp, sp
130		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
131		bic sp, sp, #0x7
132		bl exit
133		mov sp, fp
134		pop {fp, pc}
135	
136	_arrStore4:
137		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
138		push {lr}
139		cmp r10, #0
140		movlt r1, r10
141		bllt _errOutOfBounds
142		ldr lr, [r3, #-4]
143		cmp r10, lr
144		movge r1, r10
145		blge _errOutOfBounds
146		str r8, [r3, r10, lsl #2]
147		pop {pc}
148	
149	_arrLoad4:
150		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
151		push {lr}
152		cmp r10, #0
153		movlt r1, r10
154		bllt _errOutOfBounds
155		ldr lr, [r3, #-4]
156		cmp r10, lr
157		movge r1, r10
158		blge _errOutOfBounds
159		ldr r3, [r3, r10, lsl #2]
160		pop {pc}
161	
162	@ length of .L._errOutOfMemory_str0
163		.word 27
164	.L._errOutOfMemory_str0:
165		.asciz "fatal error: out of memory\n"
166	.align 4
167	_errOutOfMemory:
168		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
169		bic sp, sp, #0x7
170		adr r0, .L._errOutOfMemory_str0
171		bl _prints
172		mov r0, #255
173		bl _exit
174	
175	@ length of .L._errOutOfBounds_str0
176		.word 42
177	.L._errOutOfBounds_str0:
178		.asciz "fatal error: array index %d out of bounds\n"
179	.align 4
180	_errOutOfBounds:
181		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
182		bic sp, sp, #0x7
183		adr r0, .L._errOutOfBounds_str0
184		bl printf
185		mov r0, #0
186		bl fflush
187		mov r0, #255
188		bl _exit
===========================================================
-- Finished

