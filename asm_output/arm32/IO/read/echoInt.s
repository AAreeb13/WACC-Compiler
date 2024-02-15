./valid/IO/read/echoInt.wacc
calling the reference compiler on ./valid/IO/read/echoInt.wacc
-- Test: echoInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# echo the user's input int

# Input: 101

# Output:
# enter an integer to echo
# 101
#

# Program:

begin
	int x = 1 ;
  println "enter an integer to echo";
	read x ;
	println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
echoInt.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 24
3	.L.str0:
4		.asciz "enter an integer to echo"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #1
14		mov r4, r8
15		@ Stack pointer unchanged, no stack allocated arguments
16		ldr r8, =.L.str0
17		push {r8}
18		pop {r8}
19		mov r8, r8
20		mov r0, r8
21		@ statement primitives do not return results (but will clobber r0/rax)
22		bl _prints
23		bl _println
24		@ Stack pointer unchanged, no stack allocated arguments
25		@ load the current value in the destination of the read so it supports defaults
26		mov r8, r4
27		mov r0, r8
28		bl _readi
29		mov r12, r0
30		mov r8, r12
31		mov r4, r8
32		@ Stack pointer unchanged, no stack allocated arguments
33		mov r8, r4
34		mov r0, r8
35		@ statement primitives do not return results (but will clobber r0/rax)
36		bl _printi
37		bl _println
38		@ Stack pointer unchanged, no stack allocated variables
39		mov r0, #0
40		pop {r4, r8, r10, r12}
41		pop {fp, pc}
42	
43	@ length of .L._prints_str0
44		.word 4
45	.L._prints_str0:
46		.asciz "%.*s"
47	.align 4
48	_prints:
49		push {fp, lr}
50		mov fp, sp
51		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
52		bic sp, sp, #0x7
53		mov r2, r0
54		ldr r1, [r0, #-4]
55		adr r0, .L._prints_str0
56		bl printf
57		mov r0, #0
58		bl fflush
59		mov sp, fp
60		pop {fp, pc}
61	
62	@ length of .L._readi_str0
63		.word 2
64	.L._readi_str0:
65		.asciz "%d"
66	.align 4
67	_readi:
68		push {fp, lr}
69		mov fp, sp
70		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
71		bic sp, sp, #0x7
72		@ R0 contains the "original" value of the destination of the read
73		@ allocate space on the stack to store the read: preserve alignment!
74		@ the passed default argument should be stored in case of EOF
75		sub sp, sp, #8
76		str r0, [sp, #0]
77		mov r1, sp
78		adr r0, .L._readi_str0
79		bl scanf
80		ldr r0, [sp, #0]
81		add sp, sp, #8
82		mov sp, fp
83		pop {fp, pc}
84	
85	@ length of .L._printi_str0
86		.word 2
87	.L._printi_str0:
88		.asciz "%d"
89	.align 4
90	_printi:
91		push {fp, lr}
92		mov fp, sp
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		mov r1, r0
96		adr r0, .L._printi_str0
97		bl printf
98		mov r0, #0
99		bl fflush
100		mov sp, fp
101		pop {fp, pc}
102	
103	@ length of .L._println_str0
104		.word 0
105	.L._println_str0:
106		.asciz ""
107	.align 4
108	_println:
109		push {fp, lr}
110		mov fp, sp
111		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
112		bic sp, sp, #0x7
113		adr r0, .L._println_str0
114		bl puts
115		mov r0, #0
116		bl fflush
117		mov sp, fp
118		pop {fp, pc}
===========================================================
-- Finished

