./valid/array/charArrayInStringArray.wacc
calling the reference compiler on ./valid/array/charArrayInStringArray.wacc
-- Test: charArrayInStringArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a char[] can fit into a string[]
# Thanks to Iurii Zamiatin for finding this

# Output:

# Program:

begin
    char[] testArr = ['a', 'b', 'c'];
    string[] arr = [testArr, "box", "foo", "bar"]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charArrayInStringArray.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 3
3	.L.str0:
4		.asciz "box"
5	@ length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "foo"
9	@ length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "bar"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		@ 3 element array
22		mov r0, #7
23		bl _malloc
24		mov r12, r0
25		@ array pointers are shifted forwards by 4 bytes (to account for size)
26		add r12, r12, #4
27		mov r8, #3
28		str r8, [r12, #-4]
29		mov r8, #97
30		strb r8, [r12, #0]
31		mov r8, #98
32		strb r8, [r12, #1]
33		mov r8, #99
34		strb r8, [r12, #2]
35		mov r8, r12
36		mov r4, r8
37		@ 4 element array
38		mov r0, #20
39		bl _malloc
40		mov r12, r0
41		@ array pointers are shifted forwards by 4 bytes (to account for size)
42		add r12, r12, #4
43		mov r8, #4
44		str r8, [r12, #-4]
45		mov r8, r4
46		str r8, [r12, #0]
47		ldr r8, =.L.str0
48		push {r8}
49		pop {r8}
50		mov r8, r8
51		str r8, [r12, #4]
52		ldr r8, =.L.str1
53		push {r8}
54		pop {r8}
55		mov r8, r8
56		str r8, [r12, #8]
57		ldr r8, =.L.str2
58		push {r8}
59		pop {r8}
60		mov r8, r8
61		str r8, [r12, #12]
62		mov r8, r12
63		mov r5, r8
64		@ Stack pointer unchanged, no stack allocated variables
65		mov r0, #0
66		pop {r4, r5, r8, r10, r12}
67		pop {fp, pc}
68	
69	@ length of .L._prints_str0
70		.word 4
71	.L._prints_str0:
72		.asciz "%.*s"
73	.align 4
74	_prints:
75		push {fp, lr}
76		mov fp, sp
77		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
78		bic sp, sp, #0x7
79		mov r2, r0
80		ldr r1, [r0, #-4]
81		adr r0, .L._prints_str0
82		bl printf
83		mov r0, #0
84		bl fflush
85		mov sp, fp
86		pop {fp, pc}
87	
88	_malloc:
89		push {fp, lr}
90		mov fp, sp
91		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
92		bic sp, sp, #0x7
93		bl malloc
94		cmp r0, #0
95		bleq _errOutOfMemory
96		mov sp, fp
97		pop {fp, pc}
98	
99	_exit:
100		push {fp, lr}
101		mov fp, sp
102		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
103		bic sp, sp, #0x7
104		bl exit
105		mov sp, fp
106		pop {fp, pc}
107	
108	@ length of .L._errOutOfMemory_str0
109		.word 27
110	.L._errOutOfMemory_str0:
111		.asciz "fatal error: out of memory\n"
112	.align 4
113	_errOutOfMemory:
114		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
115		bic sp, sp, #0x7
116		adr r0, .L._errOutOfMemory_str0
117		bl _prints
118		mov r0, #255
119		bl _exit
===========================================================
-- Finished

