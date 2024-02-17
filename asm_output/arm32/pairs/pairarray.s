./valid/pairs/pairarray.wacc
calling the reference compiler on ./valid/pairs/pairarray.wacc
-- Test: pairarray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that pairs can be unpacked directly from arrays

# Output:
# 3
#

# Program:

begin
  pair(int, int) p = newpair(5, 6);
  pair(int, int)[] a = [p, p];
  fst a[0] = 3 ;
  int x = fst a[1] ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairarray.s contents are:
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
9		mov r0, #8
10		bl _malloc
11		mov r12, r0
12		mov r8, #5
13		str r8, [r12, #0]
14		mov r8, #6
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		@ 2 element array
19		mov r0, #12
20		bl _malloc
21		mov r12, r0
22		@ array pointers are shifted forwards by 4 bytes (to account for size)
23		add r12, r12, #4
24		mov r8, #2
25		str r8, [r12, #-4]
26		mov r8, r4
27		str r8, [r12, #0]
28		mov r8, r4
29		str r8, [r12, #4]
30		mov r8, r12
31		mov r5, r8
32		mov r10, #0
33		mov r3, r5
34		bl _arrLoad4
35		mov r8, r3
36		mov r8, r8
37		push {r8}
38		pop {r9}
39		cmp r9, #0
40		bleq _errNull
41		mov r8, #3
42		str r8, [r9, #0]
43		mov r10, #1
44		mov r3, r5
45		bl _arrLoad4
46		mov r8, r3
47		mov r8, r8
48		push {r8}
49		pop {r8}
50		cmp r8, #0
51		bleq _errNull
52		ldr r8, [r8, #0]
53		mov r8, r8
54		mov r6, r8
55		@ Stack pointer unchanged, no stack allocated arguments
56		mov r8, r6
57		mov r0, r8
58		@ statement primitives do not return results (but will clobber r0/rax)
59		bl _printi
60		bl _println
61		@ Stack pointer unchanged, no stack allocated variables
62		mov r0, #0
63		pop {r4, r5, r6, r8, r10, r12}
64		pop {fp, pc}
65	
66	@ length of .L._prints_str0
67		.word 4
68	.L._prints_str0:
69		.asciz "%.*s"
70	.align 4
71	_prints:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		mov r2, r0
77		ldr r1, [r0, #-4]
78		adr r0, .L._prints_str0
79		bl printf
80		mov r0, #0
81		bl fflush
82		mov sp, fp
83		pop {fp, pc}
84	
85	_malloc:
86		push {fp, lr}
87		mov fp, sp
88		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
89		bic sp, sp, #0x7
90		bl malloc
91		cmp r0, #0
92		bleq _errOutOfMemory
93		mov sp, fp
94		pop {fp, pc}
95	
96	@ length of .L._printi_str0
97		.word 2
98	.L._printi_str0:
99		.asciz "%d"
100	.align 4
101	_printi:
102		push {fp, lr}
103		mov fp, sp
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		mov r1, r0
107		adr r0, .L._printi_str0
108		bl printf
109		mov r0, #0
110		bl fflush
111		mov sp, fp
112		pop {fp, pc}
113	
114	@ length of .L._println_str0
115		.word 0
116	.L._println_str0:
117		.asciz ""
118	.align 4
119	_println:
120		push {fp, lr}
121		mov fp, sp
122		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
123		bic sp, sp, #0x7
124		adr r0, .L._println_str0
125		bl puts
126		mov r0, #0
127		bl fflush
128		mov sp, fp
129		pop {fp, pc}
130	
131	_exit:
132		push {fp, lr}
133		mov fp, sp
134		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
135		bic sp, sp, #0x7
136		bl exit
137		mov sp, fp
138		pop {fp, pc}
139	
140	_arrLoad4:
141		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
142		push {lr}
143		cmp r10, #0
144		movlt r1, r10
145		bllt _errOutOfBounds
146		ldr lr, [r3, #-4]
147		cmp r10, lr
148		movge r1, r10
149		blge _errOutOfBounds
150		ldr r3, [r3, r10, lsl #2]
151		pop {pc}
152	
153	@ length of .L._errOutOfMemory_str0
154		.word 27
155	.L._errOutOfMemory_str0:
156		.asciz "fatal error: out of memory\n"
157	.align 4
158	_errOutOfMemory:
159		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
160		bic sp, sp, #0x7
161		adr r0, .L._errOutOfMemory_str0
162		bl _prints
163		mov r0, #255
164		bl _exit
165	
166	@ length of .L._errNull_str0
167		.word 45
168	.L._errNull_str0:
169		.asciz "fatal error: null pair dereferenced or freed\n"
170	.align 4
171	_errNull:
172		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
173		bic sp, sp, #0x7
174		adr r0, .L._errNull_str0
175		bl _prints
176		mov r0, #255
177		bl _exit
178	
179	@ length of .L._errOutOfBounds_str0
180		.word 42
181	.L._errOutOfBounds_str0:
182		.asciz "fatal error: array index %d out of bounds\n"
183	.align 4
184	_errOutOfBounds:
185		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
186		bic sp, sp, #0x7
187		adr r0, .L._errOutOfBounds_str0
188		bl printf
189		mov r0, #0
190		bl fflush
191		mov r0, #255
192		bl _exit
===========================================================
-- Finished

