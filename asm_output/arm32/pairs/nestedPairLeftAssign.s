./valid/pairs/nestedPairLeftAssign.wacc
calling the reference compiler on ./valid/pairs/nestedPairLeftAssign.wacc
-- Test: nestedPairLeftAssign.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair assignments are legal as long as the right hand-side type is known

# Output:
# 7
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  fst snd q = 7 ;
  int x = fst p ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairLeftAssign.s contents are:
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
32		pop {r9}
33		cmp r9, #0
34		bleq _errNull
35		mov r8, #7
36		str r8, [r9, #0]
37		cmp r4, #0
38		bleq _errNull
39		ldr r8, [r4, #0]
40		mov r8, r8
41		mov r6, r8
42		@ Stack pointer unchanged, no stack allocated arguments
43		mov r8, r6
44		mov r0, r8
45		@ statement primitives do not return results (but will clobber r0/rax)
46		bl _printi
47		bl _println
48		@ Stack pointer unchanged, no stack allocated variables
49		mov r0, #0
50		pop {r4, r5, r6, r8, r10, r12}
51		pop {fp, pc}
52	
53	@ length of .L._prints_str0
54		.word 4
55	.L._prints_str0:
56		.asciz "%.*s"
57	.align 4
58	_prints:
59		push {fp, lr}
60		mov fp, sp
61		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
62		bic sp, sp, #0x7
63		mov r2, r0
64		ldr r1, [r0, #-4]
65		adr r0, .L._prints_str0
66		bl printf
67		mov r0, #0
68		bl fflush
69		mov sp, fp
70		pop {fp, pc}
71	
72	_malloc:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		bl malloc
78		cmp r0, #0
79		bleq _errOutOfMemory
80		mov sp, fp
81		pop {fp, pc}
82	
83	@ length of .L._printi_str0
84		.word 2
85	.L._printi_str0:
86		.asciz "%d"
87	.align 4
88	_printi:
89		push {fp, lr}
90		mov fp, sp
91		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
92		bic sp, sp, #0x7
93		mov r1, r0
94		adr r0, .L._printi_str0
95		bl printf
96		mov r0, #0
97		bl fflush
98		mov sp, fp
99		pop {fp, pc}
100	
101	@ length of .L._println_str0
102		.word 0
103	.L._println_str0:
104		.asciz ""
105	.align 4
106	_println:
107		push {fp, lr}
108		mov fp, sp
109		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
110		bic sp, sp, #0x7
111		adr r0, .L._println_str0
112		bl puts
113		mov r0, #0
114		bl fflush
115		mov sp, fp
116		pop {fp, pc}
117	
118	_exit:
119		push {fp, lr}
120		mov fp, sp
121		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
122		bic sp, sp, #0x7
123		bl exit
124		mov sp, fp
125		pop {fp, pc}
126	
127	@ length of .L._errOutOfMemory_str0
128		.word 27
129	.L._errOutOfMemory_str0:
130		.asciz "fatal error: out of memory\n"
131	.align 4
132	_errOutOfMemory:
133		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
134		bic sp, sp, #0x7
135		adr r0, .L._errOutOfMemory_str0
136		bl _prints
137		mov r0, #255
138		bl _exit
139	
140	@ length of .L._errNull_str0
141		.word 45
142	.L._errNull_str0:
143		.asciz "fatal error: null pair dereferenced or freed\n"
144	.align 4
145	_errNull:
146		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
147		bic sp, sp, #0x7
148		adr r0, .L._errNull_str0
149		bl _prints
150		mov r0, #255
151		bl _exit
===========================================================
-- Finished

