./valid/IO/IOSequence.wacc
calling the reference compiler on ./valid/IO/IOSequence.wacc
-- Test: IOSequence.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic input/output sequence

# Input: 37

# Output:
# Please input an integer: You input: 37
#

# Program:

begin
  int x = 0 ;
  print "Please input an integer: " ;
  read x ;
  print "You input: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOSequence.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 25
3	.L.str0:
4		.asciz "Please input an integer: "
5	@ length of .L.str1
6		.word 11
7	.L.str1:
8		.asciz "You input: "
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #0
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		ldr r8, =.L.str0
21		push {r8}
22		pop {r8}
23		mov r8, r8
24		mov r0, r8
25		@ statement primitives do not return results (but will clobber r0/rax)
26		bl _prints
27		@ Stack pointer unchanged, no stack allocated arguments
28		@ load the current value in the destination of the read so it supports defaults
29		mov r8, r4
30		mov r0, r8
31		bl _readi
32		mov r12, r0
33		mov r8, r12
34		mov r4, r8
35		@ Stack pointer unchanged, no stack allocated arguments
36		ldr r8, =.L.str1
37		push {r8}
38		pop {r8}
39		mov r8, r8
40		mov r0, r8
41		@ statement primitives do not return results (but will clobber r0/rax)
42		bl _prints
43		@ Stack pointer unchanged, no stack allocated arguments
44		mov r8, r4
45		mov r0, r8
46		@ statement primitives do not return results (but will clobber r0/rax)
47		bl _printi
48		bl _println
49		@ Stack pointer unchanged, no stack allocated variables
50		mov r0, #0
51		pop {r4, r8, r10, r12}
52		pop {fp, pc}
53	
54	@ length of .L._prints_str0
55		.word 4
56	.L._prints_str0:
57		.asciz "%.*s"
58	.align 4
59	_prints:
60		push {fp, lr}
61		mov fp, sp
62		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
63		bic sp, sp, #0x7
64		mov r2, r0
65		ldr r1, [r0, #-4]
66		adr r0, .L._prints_str0
67		bl printf
68		mov r0, #0
69		bl fflush
70		mov sp, fp
71		pop {fp, pc}
72	
73	@ length of .L._readi_str0
74		.word 2
75	.L._readi_str0:
76		.asciz "%d"
77	.align 4
78	_readi:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		@ R0 contains the "original" value of the destination of the read
84		@ allocate space on the stack to store the read: preserve alignment!
85		@ the passed default argument should be stored in case of EOF
86		sub sp, sp, #8
87		str r0, [sp, #0]
88		mov r1, sp
89		adr r0, .L._readi_str0
90		bl scanf
91		ldr r0, [sp, #0]
92		add sp, sp, #8
93		mov sp, fp
94		pop {fp, pc}
95	
96	@ length of .L._printi_str0
97		.word 2
98	.L._printi_str0:
99		.asciz "%d"
100	.align 4
101	_printi:
102		push {fp, lr}
103		mov fp, sp
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		mov r1, r0
107		adr r0, .L._printi_str0
108		bl printf
109		mov r0, #0
110		bl fflush
111		mov sp, fp
112		pop {fp, pc}
113	
114	@ length of .L._println_str0
115		.word 0
116	.L._println_str0:
117		.asciz ""
118	.align 4
119	_println:
120		push {fp, lr}
121		mov fp, sp
122		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
123		bic sp, sp, #0x7
124		adr r0, .L._println_str0
125		bl puts
126		mov r0, #0
127		bl fflush
128		mov sp, fp
129		pop {fp, pc}
===========================================================
-- Finished

