./valid/array/arrayLength.wacc
calling the reference compiler on ./valid/array/arrayLength.wacc
-- Test: arrayLength.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check length of array

# Output:
# 4
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println len a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLength.s contents are:
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
9		@ 4 element array
10		mov r0, #20
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #4
16		str r8, [r12, #-4]
17		mov r8, #43
18		str r8, [r12, #0]
19		mov r8, #2
20		str r8, [r12, #4]
21		mov r8, #18
22		str r8, [r12, #8]
23		mov r8, #1
24		str r8, [r12, #12]
25		mov r8, r12
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated arguments
28		ldr r8, [r4, #-4]
29		mov r8, r8
30		mov r0, r8
31		@ statement primitives do not return results (but will clobber r0/rax)
32		bl _printi
33		bl _println
34		@ Stack pointer unchanged, no stack allocated variables
35		mov r0, #0
36		pop {r4, r8, r10, r12}
37		pop {fp, pc}
38	
39	@ length of .L._prints_str0
40		.word 4
41	.L._prints_str0:
42		.asciz "%.*s"
43	.align 4
44	_prints:
45		push {fp, lr}
46		mov fp, sp
47		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
48		bic sp, sp, #0x7
49		mov r2, r0
50		ldr r1, [r0, #-4]
51		adr r0, .L._prints_str0
52		bl printf
53		mov r0, #0
54		bl fflush
55		mov sp, fp
56		pop {fp, pc}
57	
58	_malloc:
59		push {fp, lr}
60		mov fp, sp
61		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
62		bic sp, sp, #0x7
63		bl malloc
64		cmp r0, #0
65		bleq _errOutOfMemory
66		mov sp, fp
67		pop {fp, pc}
68	
69	@ length of .L._printi_str0
70		.word 2
71	.L._printi_str0:
72		.asciz "%d"
73	.align 4
74	_printi:
75		push {fp, lr}
76		mov fp, sp
77		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
78		bic sp, sp, #0x7
79		mov r1, r0
80		adr r0, .L._printi_str0
81		bl printf
82		mov r0, #0
83		bl fflush
84		mov sp, fp
85		pop {fp, pc}
86	
87	@ length of .L._println_str0
88		.word 0
89	.L._println_str0:
90		.asciz ""
91	.align 4
92	_println:
93		push {fp, lr}
94		mov fp, sp
95		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
96		bic sp, sp, #0x7
97		adr r0, .L._println_str0
98		bl puts
99		mov r0, #0
100		bl fflush
101		mov sp, fp
102		pop {fp, pc}
103	
104	_exit:
105		push {fp, lr}
106		mov fp, sp
107		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
108		bic sp, sp, #0x7
109		bl exit
110		mov sp, fp
111		pop {fp, pc}
112	
113	@ length of .L._errOutOfMemory_str0
114		.word 27
115	.L._errOutOfMemory_str0:
116		.asciz "fatal error: out of memory\n"
117	.align 4
118	_errOutOfMemory:
119		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
120		bic sp, sp, #0x7
121		adr r0, .L._errOutOfMemory_str0
122		bl _prints
123		mov r0, #255
124		bl _exit
===========================================================
-- Finished

