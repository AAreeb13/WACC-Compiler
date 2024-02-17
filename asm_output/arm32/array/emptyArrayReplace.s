./valid/array/emptyArrayReplace.wacc
calling the reference compiler on ./valid/array/emptyArrayReplace.wacc
-- Test: emptyArrayReplace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# it should be possible to reassign to an array

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  x = [0] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayReplace.s contents are:
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
9		@ 0 element array
10		mov r0, #4
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #0
16		str r8, [r12, #-4]
17		mov r8, r12
18		mov r4, r8
19		@ 1 element array
20		mov r0, #8
21		bl _malloc
22		mov r12, r0
23		@ array pointers are shifted forwards by 4 bytes (to account for size)
24		add r12, r12, #4
25		mov r8, #1
26		str r8, [r12, #-4]
27		mov r8, #0
28		str r8, [r12, #0]
29		mov r8, r12
30		mov r4, r8
31		@ Stack pointer unchanged, no stack allocated arguments
32		mov r8, #1
33		mov r0, r8
34		@ statement primitives do not return results (but will clobber r0/rax)
35		bl _printb
36		bl _println
37		@ Stack pointer unchanged, no stack allocated variables
38		mov r0, #0
39		pop {r4, r8, r10, r12}
40		pop {fp, pc}
41	
42	@ length of .L._prints_str0
43		.word 4
44	.L._prints_str0:
45		.asciz "%.*s"
46	.align 4
47	_prints:
48		push {fp, lr}
49		mov fp, sp
50		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
51		bic sp, sp, #0x7
52		mov r2, r0
53		ldr r1, [r0, #-4]
54		adr r0, .L._prints_str0
55		bl printf
56		mov r0, #0
57		bl fflush
58		mov sp, fp
59		pop {fp, pc}
60	
61	_malloc:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		bl malloc
67		cmp r0, #0
68		bleq _errOutOfMemory
69		mov sp, fp
70		pop {fp, pc}
71	
72	@ length of .L._printb_str0
73		.word 5
74	.L._printb_str0:
75		.asciz "false"
76	@ length of .L._printb_str1
77		.word 4
78	.L._printb_str1:
79		.asciz "true"
80	@ length of .L._printb_str2
81		.word 4
82	.L._printb_str2:
83		.asciz "%.*s"
84	.align 4
85	_printb:
86		push {fp, lr}
87		mov fp, sp
88		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
89		bic sp, sp, #0x7
90		cmp r0, #0
91		bne .L_printb0
92		adr r2, .L._printb_str0
93		b .L_printb1
94	.L_printb0:
95		adr r2, .L._printb_str1
96	.L_printb1:
97		ldr r1, [r2, #-4]
98		adr r0, .L._printb_str2
99		bl printf
100		mov r0, #0
101		bl fflush
102		mov sp, fp
103		pop {fp, pc}
104	
105	@ length of .L._println_str0
106		.word 0
107	.L._println_str0:
108		.asciz ""
109	.align 4
110	_println:
111		push {fp, lr}
112		mov fp, sp
113		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
114		bic sp, sp, #0x7
115		adr r0, .L._println_str0
116		bl puts
117		mov r0, #0
118		bl fflush
119		mov sp, fp
120		pop {fp, pc}
121	
122	_exit:
123		push {fp, lr}
124		mov fp, sp
125		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
126		bic sp, sp, #0x7
127		bl exit
128		mov sp, fp
129		pop {fp, pc}
130	
131	@ length of .L._errOutOfMemory_str0
132		.word 27
133	.L._errOutOfMemory_str0:
134		.asciz "fatal error: out of memory\n"
135	.align 4
136	_errOutOfMemory:
137		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
138		bic sp, sp, #0x7
139		adr r0, .L._errOutOfMemory_str0
140		bl _prints
141		mov r0, #255
142		bl _exit
===========================================================
-- Finished

