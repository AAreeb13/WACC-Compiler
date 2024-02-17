./valid/array/printRef.wacc
calling the reference compiler on ./valid/array/printRef.wacc
-- Test: printRef.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array (reference) printing

# Output:
# Printing an array variable gives an address, such as #addrs#
#

# Program:

begin
  print "Printing an array variable gives an address, such as " ;
  int[] a = [1,2,3] ;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printRef.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 53
3	.L.str0:
4		.asciz "Printing an array variable gives an address, such as "
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		@ Stack pointer unchanged, no stack allocated arguments
14		ldr r8, =.L.str0
15		push {r8}
16		pop {r8}
17		mov r8, r8
18		mov r0, r8
19		@ statement primitives do not return results (but will clobber r0/rax)
20		bl _prints
21		@ 3 element array
22		mov r0, #16
23		bl _malloc
24		mov r12, r0
25		@ array pointers are shifted forwards by 4 bytes (to account for size)
26		add r12, r12, #4
27		mov r8, #3
28		str r8, [r12, #-4]
29		mov r8, #1
30		str r8, [r12, #0]
31		mov r8, #2
32		str r8, [r12, #4]
33		mov r8, #3
34		str r8, [r12, #8]
35		mov r8, r12
36		mov r4, r8
37		@ Stack pointer unchanged, no stack allocated arguments
38		mov r8, r4
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _printp
42		bl _println
43		@ Stack pointer unchanged, no stack allocated variables
44		mov r0, #0
45		pop {r4, r8, r10, r12}
46		pop {fp, pc}
47	
48	@ length of .L._prints_str0
49		.word 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.align 4
53	_prints:
54		push {fp, lr}
55		mov fp, sp
56		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
57		bic sp, sp, #0x7
58		mov r2, r0
59		ldr r1, [r0, #-4]
60		adr r0, .L._prints_str0
61		bl printf
62		mov r0, #0
63		bl fflush
64		mov sp, fp
65		pop {fp, pc}
66	
67	@ length of .L._printp_str0
68		.word 2
69	.L._printp_str0:
70		.asciz "%p"
71	.align 4
72	_printp:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		mov r1, r0
78		adr r0, .L._printp_str0
79		bl printf
80		mov r0, #0
81		bl fflush
82		mov sp, fp
83		pop {fp, pc}
84	
85	_malloc:
86		push {fp, lr}
87		mov fp, sp
88		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
89		bic sp, sp, #0x7
90		bl malloc
91		cmp r0, #0
92		bleq _errOutOfMemory
93		mov sp, fp
94		pop {fp, pc}
95	
96	@ length of .L._println_str0
97		.word 0
98	.L._println_str0:
99		.asciz ""
100	.align 4
101	_println:
102		push {fp, lr}
103		mov fp, sp
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		adr r0, .L._println_str0
107		bl puts
108		mov r0, #0
109		bl fflush
110		mov sp, fp
111		pop {fp, pc}
112	
113	_exit:
114		push {fp, lr}
115		mov fp, sp
116		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
117		bic sp, sp, #0x7
118		bl exit
119		mov sp, fp
120		pop {fp, pc}
121	
122	@ length of .L._errOutOfMemory_str0
123		.word 27
124	.L._errOutOfMemory_str0:
125		.asciz "fatal error: out of memory\n"
126	.align 4
127	_errOutOfMemory:
128		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
129		bic sp, sp, #0x7
130		adr r0, .L._errOutOfMemory_str0
131		bl _prints
132		mov r0, #255
133		bl _exit
===========================================================
-- Finished

