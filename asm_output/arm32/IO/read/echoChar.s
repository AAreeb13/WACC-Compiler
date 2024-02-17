./valid/IO/read/echoChar.wacc
calling the reference compiler on ./valid/IO/read/echoChar.wacc
-- Test: echoChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# echo the user's input char

# Input: K

# Output:
# enter a character to echo
# K
#

# Program:

begin
	char c = '\0' ;
  println "enter a character to echo";
	read c ;
	println c
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
echoChar.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 25
3	.L.str0:
4		.asciz "enter a character to echo"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #0
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
28		bl _readc
29		mov r12, r0
30		mov r8, r12
31		mov r4, r8
32		@ Stack pointer unchanged, no stack allocated arguments
33		mov r8, r4
34		mov r0, r8
35		@ statement primitives do not return results (but will clobber r0/rax)
36		bl _printc
37		bl _println
38		@ Stack pointer unchanged, no stack allocated variables
39		mov r0, #0
40		pop {r4, r8, r10, r12}
41		pop {fp, pc}
42	
43	@ length of .L._readc_str0
44		.word 3
45	.L._readc_str0:
46		.asciz " %c"
47	.align 4
48	_readc:
49		push {fp, lr}
50		mov fp, sp
51		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
52		bic sp, sp, #0x7
53		@ R0 contains the "original" value of the destination of the read
54		@ allocate space on the stack to store the read: preserve alignment!
55		@ the passed default argument should be stored in case of EOF
56		sub sp, sp, #8
57		strb r0, [sp, #0]
58		mov r1, sp
59		adr r0, .L._readc_str0
60		bl scanf
61		ldrsb r0, [sp, #0]
62		add sp, sp, #8
63		mov sp, fp
64		pop {fp, pc}
65	
66	@ length of .L._prints_str0
67		.word 4
68	.L._prints_str0:
69		.asciz "%.*s"
70	.align 4
71	_prints:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		mov r2, r0
77		ldr r1, [r0, #-4]
78		adr r0, .L._prints_str0
79		bl printf
80		mov r0, #0
81		bl fflush
82		mov sp, fp
83		pop {fp, pc}
84	
85	@ length of .L._printc_str0
86		.word 2
87	.L._printc_str0:
88		.asciz "%c"
89	.align 4
90	_printc:
91		push {fp, lr}
92		mov fp, sp
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		mov r1, r0
96		adr r0, .L._printc_str0
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

