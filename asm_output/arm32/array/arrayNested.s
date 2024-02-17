./valid/array/arrayNested.wacc
calling the reference compiler on ./valid/array/arrayNested.wacc
-- Test: arrayNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:
# 3
# 3
#

# Program:

begin
  int[] a = [1,2,3];
  int[] b = [3,4];
  int[][] c = [a,b] ;
  println c[0][2] ;
  println c[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayNested.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r8, r10, r12}
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
25		@ 2 element array
26		mov r0, #12
27		bl _malloc
28		mov r12, r0
29		@ array pointers are shifted forwards by 4 bytes (to account for size)
30		add r12, r12, #4
31		mov r8, #2
32		str r8, [r12, #-4]
33		mov r8, #3
34		str r8, [r12, #0]
35		mov r8, #4
36		str r8, [r12, #4]
37		mov r8, r12
38		mov r5, r8
39		@ 2 element array
40		mov r0, #12
41		bl _malloc
42		mov r12, r0
43		@ array pointers are shifted forwards by 4 bytes (to account for size)
44		add r12, r12, #4
45		mov r8, #2
46		str r8, [r12, #-4]
47		mov r8, r4
48		str r8, [r12, #0]
49		mov r8, r5
50		str r8, [r12, #4]
51		mov r8, r12
52		mov r6, r8
53		@ Stack pointer unchanged, no stack allocated arguments
54		mov r10, #0
55		mov r3, r6
56		bl _arrLoad4
57		mov r8, r3
58		mov r8, r8
59		push {r8}
60		mov r10, #2
61		pop {r8}
62		mov r3, r8
63		bl _arrLoad4
64		mov r8, r3
65		mov r8, r8
66		mov r0, r8
67		@ statement primitives do not return results (but will clobber r0/rax)
68		bl _printi
69		bl _println
70		@ Stack pointer unchanged, no stack allocated arguments
71		mov r10, #1
72		mov r3, r6
73		bl _arrLoad4
74		mov r8, r3
75		mov r8, r8
76		push {r8}
77		mov r10, #0
78		pop {r8}
79		mov r3, r8
80		bl _arrLoad4
81		mov r8, r3
82		mov r8, r8
83		mov r0, r8
84		@ statement primitives do not return results (but will clobber r0/rax)
85		bl _printi
86		bl _println
87		@ Stack pointer unchanged, no stack allocated variables
88		mov r0, #0
89		pop {r4, r5, r6, r8, r10, r12}
90		pop {fp, pc}
91	
92	@ length of .L._prints_str0
93		.word 4
94	.L._prints_str0:
95		.asciz "%.*s"
96	.align 4
97	_prints:
98		push {fp, lr}
99		mov fp, sp
100		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
101		bic sp, sp, #0x7
102		mov r2, r0
103		ldr r1, [r0, #-4]
104		adr r0, .L._prints_str0
105		bl printf
106		mov r0, #0
107		bl fflush
108		mov sp, fp
109		pop {fp, pc}
110	
111	_malloc:
112		push {fp, lr}
113		mov fp, sp
114		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
115		bic sp, sp, #0x7
116		bl malloc
117		cmp r0, #0
118		bleq _errOutOfMemory
119		mov sp, fp
120		pop {fp, pc}
121	
122	@ length of .L._printi_str0
123		.word 2
124	.L._printi_str0:
125		.asciz "%d"
126	.align 4
127	_printi:
128		push {fp, lr}
129		mov fp, sp
130		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
131		bic sp, sp, #0x7
132		mov r1, r0
133		adr r0, .L._printi_str0
134		bl printf
135		mov r0, #0
136		bl fflush
137		mov sp, fp
138		pop {fp, pc}
139	
140	@ length of .L._println_str0
141		.word 0
142	.L._println_str0:
143		.asciz ""
144	.align 4
145	_println:
146		push {fp, lr}
147		mov fp, sp
148		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
149		bic sp, sp, #0x7
150		adr r0, .L._println_str0
151		bl puts
152		mov r0, #0
153		bl fflush
154		mov sp, fp
155		pop {fp, pc}
156	
157	_exit:
158		push {fp, lr}
159		mov fp, sp
160		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
161		bic sp, sp, #0x7
162		bl exit
163		mov sp, fp
164		pop {fp, pc}
165	
166	_arrLoad4:
167		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
168		push {lr}
169		cmp r10, #0
170		movlt r1, r10
171		bllt _errOutOfBounds
172		ldr lr, [r3, #-4]
173		cmp r10, lr
174		movge r1, r10
175		blge _errOutOfBounds
176		ldr r3, [r3, r10, lsl #2]
177		pop {pc}
178	
179	@ length of .L._errOutOfMemory_str0
180		.word 27
181	.L._errOutOfMemory_str0:
182		.asciz "fatal error: out of memory\n"
183	.align 4
184	_errOutOfMemory:
185		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
186		bic sp, sp, #0x7
187		adr r0, .L._errOutOfMemory_str0
188		bl _prints
189		mov r0, #255
190		bl _exit
191	
192	@ length of .L._errOutOfBounds_str0
193		.word 42
194	.L._errOutOfBounds_str0:
195		.asciz "fatal error: array index %d out of bounds\n"
196	.align 4
197	_errOutOfBounds:
198		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
199		bic sp, sp, #0x7
200		adr r0, .L._errOutOfBounds_str0
201		bl printf
202		mov r0, #0
203		bl fflush
204		mov r0, #255
205		bl _exit
===========================================================
-- Finished

