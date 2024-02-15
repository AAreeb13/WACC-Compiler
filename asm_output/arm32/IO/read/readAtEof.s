./valid/IO/read/readAtEof.wacc
calling the reference compiler on ./valid/IO/read/readAtEof.wacc
-- Test: readAtEof.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Read should not alter nothing if it can't read anything
# Thanks for Nixon Moony-Enraght for catching this!

# Input: X

# Output:
# XZ
#

# Program:
begin
    char c = 'Z';
    read c;
    print c;

    char c2 = 'Z';
    read c2;
    println c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readAtEof.s contents are:
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
9		mov r8, #90
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		@ load the current value in the destination of the read so it supports defaults
13		mov r8, r4
14		mov r0, r8
15		bl _readc
16		mov r12, r0
17		mov r8, r12
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, r4
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printc
24		mov r8, #90
25		mov r5, r8
26		@ Stack pointer unchanged, no stack allocated arguments
27		@ load the current value in the destination of the read so it supports defaults
28		mov r8, r5
29		mov r0, r8
30		bl _readc
31		mov r12, r0
32		mov r8, r12
33		mov r5, r8
34		@ Stack pointer unchanged, no stack allocated arguments
35		mov r8, r5
36		mov r0, r8
37		@ statement primitives do not return results (but will clobber r0/rax)
38		bl _printc
39		bl _println
40		@ Stack pointer unchanged, no stack allocated variables
41		mov r0, #0
42		pop {r4, r5, r8, r10, r12}
43		pop {fp, pc}
44	
45	@ length of .L._readc_str0
46		.word 3
47	.L._readc_str0:
48		.asciz " %c"
49	.align 4
50	_readc:
51		push {fp, lr}
52		mov fp, sp
53		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
54		bic sp, sp, #0x7
55		@ R0 contains the "original" value of the destination of the read
56		@ allocate space on the stack to store the read: preserve alignment!
57		@ the passed default argument should be stored in case of EOF
58		sub sp, sp, #8
59		strb r0, [sp, #0]
60		mov r1, sp
61		adr r0, .L._readc_str0
62		bl scanf
63		ldrsb r0, [sp, #0]
64		add sp, sp, #8
65		mov sp, fp
66		pop {fp, pc}
67	
68	@ length of .L._printc_str0
69		.word 2
70	.L._printc_str0:
71		.asciz "%c"
72	.align 4
73	_printc:
74		push {fp, lr}
75		mov fp, sp
76		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
77		bic sp, sp, #0x7
78		mov r1, r0
79		adr r0, .L._printc_str0
80		bl printf
81		mov r0, #0
82		bl fflush
83		mov sp, fp
84		pop {fp, pc}
85	
86	@ length of .L._println_str0
87		.word 0
88	.L._println_str0:
89		.asciz ""
90	.align 4
91	_println:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		adr r0, .L._println_str0
97		bl puts
98		mov r0, #0
99		bl fflush
100		mov sp, fp
101		pop {fp, pc}
===========================================================
-- Finished

