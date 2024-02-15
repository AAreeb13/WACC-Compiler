./valid/pairs/free.wacc
calling the reference compiler on ./valid/pairs/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a simple pair

# Output:

# Program:

begin
  pair(int, char) a = newpair(10, 'a') ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
9		mov r0, #8
10		bl _malloc
11		mov r12, r0
12		mov r8, #10
13		str r8, [r12, #0]
14		mov r8, #97
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		@ Stack pointer unchanged, no stack allocated arguments
19		mov r8, r4
20		mov r0, r8
21		@ statement primitives do not return results (but will clobber r0/rax)
22		bl _freepair
23		@ Stack pointer unchanged, no stack allocated variables
24		mov r0, #0
25		pop {r4, r8, r10, r12}
26		pop {fp, pc}
27	
28	_freepair:
29		push {fp, lr}
30		mov fp, sp
31		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
32		bic sp, sp, #0x7
33		cmp r0, #0
34		bleq _errNull
35		bl free
36		mov sp, fp
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
69	_exit:
70		push {fp, lr}
71		mov fp, sp
72		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
73		bic sp, sp, #0x7
74		bl exit
75		mov sp, fp
76		pop {fp, pc}
77	
78	@ length of .L._errOutOfMemory_str0
79		.word 27
80	.L._errOutOfMemory_str0:
81		.asciz "fatal error: out of memory\n"
82	.align 4
83	_errOutOfMemory:
84		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
85		bic sp, sp, #0x7
86		adr r0, .L._errOutOfMemory_str0
87		bl _prints
88		mov r0, #255
89		bl _exit
90	
91	@ length of .L._errNull_str0
92		.word 45
93	.L._errNull_str0:
94		.asciz "fatal error: null pair dereferenced or freed\n"
95	.align 4
96	_errNull:
97		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
98		bic sp, sp, #0x7
99		adr r0, .L._errNull_str0
100		bl _prints
101		mov r0, #255
102		bl _exit
===========================================================
-- Finished

