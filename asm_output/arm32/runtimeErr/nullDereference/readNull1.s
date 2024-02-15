./valid/runtimeErr/nullDereference/readNull1.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/readNull1.wacc
-- Test: readNull1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by reading into an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	read fst p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readNull1.s contents are:
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
9		mov r8, #0
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		@ load the current value in the destination of the read so it supports defaults
13		cmp r4, #0
14		bleq _errNull
15		ldr r8, [r4, #0]
16		mov r8, r8
17		mov r0, r8
18		bl _readi
19		mov r12, r0
20		cmp r4, #0
21		bleq _errNull
22		mov r8, r12
23		str r8, [r4, #0]
24		@ Stack pointer unchanged, no stack allocated variables
25		mov r0, #0
26		pop {r4, r8, r10, r12}
27		pop {fp, pc}
28	
29	@ length of .L._prints_str0
30		.word 4
31	.L._prints_str0:
32		.asciz "%.*s"
33	.align 4
34	_prints:
35		push {fp, lr}
36		mov fp, sp
37		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
38		bic sp, sp, #0x7
39		mov r2, r0
40		ldr r1, [r0, #-4]
41		adr r0, .L._prints_str0
42		bl printf
43		mov r0, #0
44		bl fflush
45		mov sp, fp
46		pop {fp, pc}
47	
48	@ length of .L._readi_str0
49		.word 2
50	.L._readi_str0:
51		.asciz "%d"
52	.align 4
53	_readi:
54		push {fp, lr}
55		mov fp, sp
56		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
57		bic sp, sp, #0x7
58		@ R0 contains the "original" value of the destination of the read
59		@ allocate space on the stack to store the read: preserve alignment!
60		@ the passed default argument should be stored in case of EOF
61		sub sp, sp, #8
62		str r0, [sp, #0]
63		mov r1, sp
64		adr r0, .L._readi_str0
65		bl scanf
66		ldr r0, [sp, #0]
67		add sp, sp, #8
68		mov sp, fp
69		pop {fp, pc}
70	
71	_exit:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		bl exit
77		mov sp, fp
78		pop {fp, pc}
79	
80	@ length of .L._errNull_str0
81		.word 45
82	.L._errNull_str0:
83		.asciz "fatal error: null pair dereferenced or freed\n"
84	.align 4
85	_errNull:
86		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
87		bic sp, sp, #0x7
88		adr r0, .L._errNull_str0
89		bl _prints
90		mov r0, #255
91		bl _exit
===========================================================
-- Finished

