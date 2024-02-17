./valid/array/emptyArrayPrint.wacc
calling the reference compiler on ./valid/array/emptyArrayPrint.wacc
-- Test: emptyArrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and print after

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayPrint.s contents are:
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
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, #1
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printb
24		bl _println
25		@ Stack pointer unchanged, no stack allocated variables
26		mov r0, #0
27		pop {r4, r8, r10, r12}
28		pop {fp, pc}
29	
30	@ length of .L._prints_str0
31		.word 4
32	.L._prints_str0:
33		.asciz "%.*s"
34	.align 4
35	_prints:
36		push {fp, lr}
37		mov fp, sp
38		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
39		bic sp, sp, #0x7
40		mov r2, r0
41		ldr r1, [r0, #-4]
42		adr r0, .L._prints_str0
43		bl printf
44		mov r0, #0
45		bl fflush
46		mov sp, fp
47		pop {fp, pc}
48	
49	_malloc:
50		push {fp, lr}
51		mov fp, sp
52		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
53		bic sp, sp, #0x7
54		bl malloc
55		cmp r0, #0
56		bleq _errOutOfMemory
57		mov sp, fp
58		pop {fp, pc}
59	
60	@ length of .L._printb_str0
61		.word 5
62	.L._printb_str0:
63		.asciz "false"
64	@ length of .L._printb_str1
65		.word 4
66	.L._printb_str1:
67		.asciz "true"
68	@ length of .L._printb_str2
69		.word 4
70	.L._printb_str2:
71		.asciz "%.*s"
72	.align 4
73	_printb:
74		push {fp, lr}
75		mov fp, sp
76		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
77		bic sp, sp, #0x7
78		cmp r0, #0
79		bne .L_printb0
80		adr r2, .L._printb_str0
81		b .L_printb1
82	.L_printb0:
83		adr r2, .L._printb_str1
84	.L_printb1:
85		ldr r1, [r2, #-4]
86		adr r0, .L._printb_str2
87		bl printf
88		mov r0, #0
89		bl fflush
90		mov sp, fp
91		pop {fp, pc}
92	
93	@ length of .L._println_str0
94		.word 0
95	.L._println_str0:
96		.asciz ""
97	.align 4
98	_println:
99		push {fp, lr}
100		mov fp, sp
101		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
102		bic sp, sp, #0x7
103		adr r0, .L._println_str0
104		bl puts
105		mov r0, #0
106		bl fflush
107		mov sp, fp
108		pop {fp, pc}
109	
110	_exit:
111		push {fp, lr}
112		mov fp, sp
113		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
114		bic sp, sp, #0x7
115		bl exit
116		mov sp, fp
117		pop {fp, pc}
118	
119	@ length of .L._errOutOfMemory_str0
120		.word 27
121	.L._errOutOfMemory_str0:
122		.asciz "fatal error: out of memory\n"
123	.align 4
124	_errOutOfMemory:
125		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
126		bic sp, sp, #0x7
127		adr r0, .L._errOutOfMemory_str0
128		bl _prints
129		mov r0, #255
130		bl _exit
===========================================================
-- Finished

