./valid/array/modifyString.wacc
calling the reference compiler on ./valid/array/modifyString.wacc
-- Test: modifyString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and modify string as array of characters

# Output:
# hello world!
# Hello world!
# Hi!
#

# Program:

begin
  char[] str = ['h','e','l','l','o',' ','w','o','r','l','d','!'] ;
  println str ;
  str[0] = 'H' ;
  println str ;
  str = ['H','i','!'] ;
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
modifyString.s contents are:
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
9		@ 12 element array
10		mov r0, #16
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #12
16		str r8, [r12, #-4]
17		mov r8, #104
18		strb r8, [r12, #0]
19		mov r8, #101
20		strb r8, [r12, #1]
21		mov r8, #108
22		strb r8, [r12, #2]
23		mov r8, #108
24		strb r8, [r12, #3]
25		mov r8, #111
26		strb r8, [r12, #4]
27		mov r8, #32
28		strb r8, [r12, #5]
29		mov r8, #119
30		strb r8, [r12, #6]
31		mov r8, #111
32		strb r8, [r12, #7]
33		mov r8, #114
34		strb r8, [r12, #8]
35		mov r8, #108
36		strb r8, [r12, #9]
37		mov r8, #100
38		strb r8, [r12, #10]
39		mov r8, #33
40		strb r8, [r12, #11]
41		mov r8, r12
42		mov r4, r8
43		@ Stack pointer unchanged, no stack allocated arguments
44		mov r8, r4
45		mov r0, r8
46		@ statement primitives do not return results (but will clobber r0/rax)
47		bl _prints
48		bl _println
49		mov r10, #0
50		mov r8, #72
51		mov r3, r4
52		bl _arrStore1
53		@ Stack pointer unchanged, no stack allocated arguments
54		mov r8, r4
55		mov r0, r8
56		@ statement primitives do not return results (but will clobber r0/rax)
57		bl _prints
58		bl _println
59		@ 3 element array
60		mov r0, #7
61		bl _malloc
62		mov r12, r0
63		@ array pointers are shifted forwards by 4 bytes (to account for size)
64		add r12, r12, #4
65		mov r8, #3
66		str r8, [r12, #-4]
67		mov r8, #72
68		strb r8, [r12, #0]
69		mov r8, #105
70		strb r8, [r12, #1]
71		mov r8, #33
72		strb r8, [r12, #2]
73		mov r8, r12
74		mov r4, r8
75		@ Stack pointer unchanged, no stack allocated arguments
76		mov r8, r4
77		mov r0, r8
78		@ statement primitives do not return results (but will clobber r0/rax)
79		bl _prints
80		bl _println
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
105	_malloc:
106		push {fp, lr}
107		mov fp, sp
108		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
109		bic sp, sp, #0x7
110		bl malloc
111		cmp r0, #0
112		bleq _errOutOfMemory
113		mov sp, fp
114		pop {fp, pc}
115	
116	@ length of .L._println_str0
117		.word 0
118	.L._println_str0:
119		.asciz ""
120	.align 4
121	_println:
122		push {fp, lr}
123		mov fp, sp
124		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
125		bic sp, sp, #0x7
126		adr r0, .L._println_str0
127		bl puts
128		mov r0, #0
129		bl fflush
130		mov sp, fp
131		pop {fp, pc}
132	
133	_exit:
134		push {fp, lr}
135		mov fp, sp
136		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
137		bic sp, sp, #0x7
138		bl exit
139		mov sp, fp
140		pop {fp, pc}
141	
142	_arrStore1:
143		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
144		push {lr}
145		cmp r10, #0
146		movlt r1, r10
147		bllt _errOutOfBounds
148		ldr lr, [r3, #-4]
149		cmp r10, lr
150		movge r1, r10
151		blge _errOutOfBounds
152		strb r8, [r3, r10]
153		pop {pc}
154	
155	@ length of .L._errOutOfMemory_str0
156		.word 27
157	.L._errOutOfMemory_str0:
158		.asciz "fatal error: out of memory\n"
159	.align 4
160	_errOutOfMemory:
161		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
162		bic sp, sp, #0x7
163		adr r0, .L._errOutOfMemory_str0
164		bl _prints
165		mov r0, #255
166		bl _exit
167	
168	@ length of .L._errOutOfBounds_str0
169		.word 42
170	.L._errOutOfBounds_str0:
171		.asciz "fatal error: array index %d out of bounds\n"
172	.align 4
173	_errOutOfBounds:
174		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
175		bic sp, sp, #0x7
176		adr r0, .L._errOutOfBounds_str0
177		bl printf
178		mov r0, #0
179		bl fflush
180		mov r0, #255
181		bl _exit
===========================================================
-- Finished

