./valid/pairs/nestedPairRightExtract.wacc
calling the reference compiler on ./valid/pairs/nestedPairRightExtract.wacc
-- Test: nestedPairRightExtract.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair extractions are legal as long as the left hand-side type is known

# Output:
# 2
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  int x = fst snd q ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairRightExtract.s contents are:
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
12		mov r8, #2
13		str r8, [r12, #0]
14		mov r8, #3
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		mov r0, #8
19		bl _malloc
20		mov r12, r0
21		mov r8, #1
22		str r8, [r12, #0]
23		mov r8, r4
24		str r8, [r12, #4]
25		mov r8, r12
26		mov r5, r8
27		cmp r5, #0
28		bleq _errNull
29		ldr r8, [r5, #4]
30		mov r8, r8
31		push {r8}
32		pop {r8}
33		cmp r8, #0
34		bleq _errNull
35		ldr r8, [r8, #0]
36		mov r8, r8
37		mov r6, r8
38		@ Stack pointer unchanged, no stack allocated arguments
39		mov r8, r6
40		mov r0, r8
41		@ statement primitives do not return results (but will clobber r0/rax)
42		bl _printi
43		bl _println
44		@ Stack pointer unchanged, no stack allocated variables
45		mov r0, #0
46		pop {r4, r5, r6, r8, r10, r12}
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
97	@ length of .L._println_str0
98		.word 0
99	.L._println_str0:
100		.asciz ""
101	.align 4
102	_println:
103		push {fp, lr}
104		mov fp, sp
105		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
106		bic sp, sp, #0x7
107		adr r0, .L._println_str0
108		bl puts
109		mov r0, #0
110		bl fflush
111		mov sp, fp
112		pop {fp, pc}
113	
114	_exit:
115		push {fp, lr}
116		mov fp, sp
117		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
118		bic sp, sp, #0x7
119		bl exit
120		mov sp, fp
121		pop {fp, pc}
122	
123	@ length of .L._errOutOfMemory_str0
124		.word 27
125	.L._errOutOfMemory_str0:
126		.asciz "fatal error: out of memory\n"
127	.align 4
128	_errOutOfMemory:
129		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
130		bic sp, sp, #0x7
131		adr r0, .L._errOutOfMemory_str0
132		bl _prints
133		mov r0, #255
134		bl _exit
135	
136	@ length of .L._errNull_str0
137		.word 45
138	.L._errNull_str0:
139		.asciz "fatal error: null pair dereferenced or freed\n"
140	.align 4
141	_errNull:
142		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
143		bic sp, sp, #0x7
144		adr r0, .L._errNull_str0
145		bl _prints
146		mov r0, #255
147		bl _exit
===========================================================
-- Finished

