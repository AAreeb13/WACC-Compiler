./valid/IO/read/read.wacc
calling the reference compiler on ./valid/IO/read/read.wacc
-- Test: read.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple read statement

#Input: 350

# Output:
# input an integer to continue...
#

# Program:

begin
	int x = 10;
  println "input an integer to continue..." ;
	read x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
read.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 31
3	.L.str0:
4		.asciz "input an integer to continue..."
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #10
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
32		@ Stack pointer unchanged, no stack allocated variables
33		mov r0, #0
34		pop {r4, r8, r10, r12}
35		pop {fp, pc}
36	
37	@ length of .L._prints_str0
38		.word 4
39	.L._prints_str0:
40		.asciz "%.*s"
41	.align 4
42	_prints:
43		push {fp, lr}
44		mov fp, sp
45		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
46		bic sp, sp, #0x7
47		mov r2, r0
48		ldr r1, [r0, #-4]
49		adr r0, .L._prints_str0
50		bl printf
51		mov r0, #0
52		bl fflush
53		mov sp, fp
54		pop {fp, pc}
55	
56	@ length of .L._readi_str0
57		.word 2
58	.L._readi_str0:
59		.asciz "%d"
60	.align 4
61	_readi:
62		push {fp, lr}
63		mov fp, sp
64		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
65		bic sp, sp, #0x7
66		@ R0 contains the "original" value of the destination of the read
67		@ allocate space on the stack to store the read: preserve alignment!
68		@ the passed default argument should be stored in case of EOF
69		sub sp, sp, #8
70		str r0, [sp, #0]
71		mov r1, sp
72		adr r0, .L._readi_str0
73		bl scanf
74		ldr r0, [sp, #0]
75		add sp, sp, #8
76		mov sp, fp
77		pop {fp, pc}
78	
79	@ length of .L._println_str0
80		.word 0
81	.L._println_str0:
82		.asciz ""
83	.align 4
84	_println:
85		push {fp, lr}
86		mov fp, sp
87		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
88		bic sp, sp, #0x7
89		adr r0, .L._println_str0
90		bl puts
91		mov r0, #0
92		bl fflush
93		mov sp, fp
94		pop {fp, pc}
===========================================================
-- Finished

