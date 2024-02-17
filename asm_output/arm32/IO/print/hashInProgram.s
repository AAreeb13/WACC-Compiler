./valid/IO/print/hashInProgram.wacc
calling the reference compiler on ./valid/IO/print/hashInProgram.wacc
-- Test: hashInProgram.wacc

-- Uploaded file: 
---------------------------------------------------------------
# In-line comments and printing #

# Output:
# We can print the hash character: #
# We can also print # when its in a string.
#

# Program:

begin
  int x = 0 ; # comments can be in-line
  print "We can print the hash character: " ;
  println '#' ;
  println "We can also print # when its in a string."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
hashInProgram.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 33
3	.L.str0:
4		.asciz "We can print the hash character: "
5	@ length of .L.str1
6		.word 41
7	.L.str1:
8		.asciz "We can also print # when its in a string."
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
28		mov r8, #35
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _printc
32		bl _println
33		@ Stack pointer unchanged, no stack allocated arguments
34		ldr r8, =.L.str1
35		push {r8}
36		pop {r8}
37		mov r8, r8
38		mov r0, r8
39		@ statement primitives do not return results (but will clobber r0/rax)
40		bl _prints
41		bl _println
42		@ Stack pointer unchanged, no stack allocated variables
43		mov r0, #0
44		pop {r4, r8, r10, r12}
45		pop {fp, pc}
46	
47	@ length of .L._prints_str0
48		.word 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.align 4
52	_prints:
53		push {fp, lr}
54		mov fp, sp
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		mov r2, r0
58		ldr r1, [r0, #-4]
59		adr r0, .L._prints_str0
60		bl printf
61		mov r0, #0
62		bl fflush
63		mov sp, fp
64		pop {fp, pc}
65	
66	@ length of .L._printc_str0
67		.word 2
68	.L._printc_str0:
69		.asciz "%c"
70	.align 4
71	_printc:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		mov r1, r0
77		adr r0, .L._printc_str0
78		bl printf
79		mov r0, #0
80		bl fflush
81		mov sp, fp
82		pop {fp, pc}
83	
84	@ length of .L._println_str0
85		.word 0
86	.L._println_str0:
87		.asciz ""
88	.align 4
89	_println:
90		push {fp, lr}
91		mov fp, sp
92		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
93		bic sp, sp, #0x7
94		adr r0, .L._println_str0
95		bl puts
96		mov r0, #0
97		bl fflush
98		mov sp, fp
99		pop {fp, pc}
===========================================================
-- Finished

