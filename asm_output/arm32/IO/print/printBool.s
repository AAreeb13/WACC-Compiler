./valid/IO/print/printBool.wacc
calling the reference compiler on ./valid/IO/print/printBool.wacc
-- Test: printBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic Boolean printing

# Output:
# True is true
# False is false
#

# Program:

begin
  print "True is " ;
  println true ;

  print "False is " ;
  println false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printBool.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "True is "
5	@ length of .L.str1
6		.word 9
7	.L.str1:
8		.asciz "False is "
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated arguments
17		ldr r8, =.L.str0
18		push {r8}
19		pop {r8}
20		mov r8, r8
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _prints
24		@ Stack pointer unchanged, no stack allocated arguments
25		mov r8, #1
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _printb
29		bl _println
30		@ Stack pointer unchanged, no stack allocated arguments
31		ldr r8, =.L.str1
32		push {r8}
33		pop {r8}
34		mov r8, r8
35		mov r0, r8
36		@ statement primitives do not return results (but will clobber r0/rax)
37		bl _prints
38		@ Stack pointer unchanged, no stack allocated arguments
39		mov r8, #0
40		mov r0, r8
41		@ statement primitives do not return results (but will clobber r0/rax)
42		bl _printb
43		bl _println
44		mov r0, #0
45		pop {r8, r10, r12}
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
67	@ length of .L._printb_str0
68		.word 5
69	.L._printb_str0:
70		.asciz "false"
71	@ length of .L._printb_str1
72		.word 4
73	.L._printb_str1:
74		.asciz "true"
75	@ length of .L._printb_str2
76		.word 4
77	.L._printb_str2:
78		.asciz "%.*s"
79	.align 4
80	_printb:
81		push {fp, lr}
82		mov fp, sp
83		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
84		bic sp, sp, #0x7
85		cmp r0, #0
86		bne .L_printb0
87		adr r2, .L._printb_str0
88		b .L_printb1
89	.L_printb0:
90		adr r2, .L._printb_str1
91	.L_printb1:
92		ldr r1, [r2, #-4]
93		adr r0, .L._printb_str2
94		bl printf
95		mov r0, #0
96		bl fflush
97		mov sp, fp
98		pop {fp, pc}
99	
100	@ length of .L._println_str0
101		.word 0
102	.L._println_str0:
103		.asciz ""
104	.align 4
105	_println:
106		push {fp, lr}
107		mov fp, sp
108		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
109		bic sp, sp, #0x7
110		adr r0, .L._println_str0
111		bl puts
112		mov r0, #0
113		bl fflush
114		mov sp, fp
115		pop {fp, pc}
===========================================================
-- Finished

