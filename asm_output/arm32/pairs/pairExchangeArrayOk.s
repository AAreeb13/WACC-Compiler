./valid/pairs/pairExchangeArrayOk.wacc
calling the reference compiler on ./valid/pairs/pairExchangeArrayOk.wacc
-- Test: pairExchangeArrayOk.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Assignment is legal when assigning array (even of unknown type) in nested pair extraction
# Thanks to Kristina Zimina for catching this

# Output:

# Program:

begin
  pair(int, int) p = newpair(4, 5);
  pair(pair, int) q = newpair(p, 6);
  fst fst q = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairExchangeArrayOk.s contents are:
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
9		mov r0, #8
10		bl _malloc
11		mov r12, r0
12		mov r8, #4
13		str r8, [r12, #0]
14		mov r8, #5
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		mov r0, #8
19		bl _malloc
20		mov r12, r0
21		mov r8, r4
22		str r8, [r12, #0]
23		mov r8, #6
24		str r8, [r12, #4]
25		mov r8, r12
26		mov r5, r8
27		@ 0 element array
28		mov r0, #4
29		bl _malloc
30		mov r12, r0
31		@ array pointers are shifted forwards by 4 bytes (to account for size)
32		add r12, r12, #4
33		mov r8, #0
34		str r8, [r12, #-4]
35		cmp r5, #0
36		bleq _errNull
37		ldr r8, [r5, #0]
38		mov r8, r8
39		push {r8}
40		pop {r9}
41		cmp r9, #0
42		bleq _errNull
43		mov r8, r12
44		str r8, [r9, #0]
45		@ Stack pointer unchanged, no stack allocated variables
46		mov r0, #0
47		pop {r4, r5, r8, r10, r12}
48		pop {fp, pc}
49	
50	@ length of .L._prints_str0
51		.word 4
52	.L._prints_str0:
53		.asciz "%.*s"
54	.align 4
55	_prints:
56		push {fp, lr}
57		mov fp, sp
58		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
59		bic sp, sp, #0x7
60		mov r2, r0
61		ldr r1, [r0, #-4]
62		adr r0, .L._prints_str0
63		bl printf
64		mov r0, #0
65		bl fflush
66		mov sp, fp
67		pop {fp, pc}
68	
69	_malloc:
70		push {fp, lr}
71		mov fp, sp
72		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
73		bic sp, sp, #0x7
74		bl malloc
75		cmp r0, #0
76		bleq _errOutOfMemory
77		mov sp, fp
78		pop {fp, pc}
79	
80	_exit:
81		push {fp, lr}
82		mov fp, sp
83		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
84		bic sp, sp, #0x7
85		bl exit
86		mov sp, fp
87		pop {fp, pc}
88	
89	@ length of .L._errOutOfMemory_str0
90		.word 27
91	.L._errOutOfMemory_str0:
92		.asciz "fatal error: out of memory\n"
93	.align 4
94	_errOutOfMemory:
95		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
96		bic sp, sp, #0x7
97		adr r0, .L._errOutOfMemory_str0
98		bl _prints
99		mov r0, #255
100		bl _exit
101	
102	@ length of .L._errNull_str0
103		.word 45
104	.L._errNull_str0:
105		.asciz "fatal error: null pair dereferenced or freed\n"
106	.align 4
107	_errNull:
108		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
109		bic sp, sp, #0x7
110		adr r0, .L._errNull_str0
111		bl _prints
112		mov r0, #255
113		bl _exit
===========================================================
-- Finished

