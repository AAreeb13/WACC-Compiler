./valid/runtimeErr/divideByZero/divZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divZero.wacc
-- Test: divZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 10 / 0 ;
  println "should not reach here"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divZero.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 21
3	.L.str0:
4		.asciz "should not reach here"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r0, #10
14		mov r1, #0
15		cmp r1, #0
16		bleq _errDivZero
17		bl __aeabi_idivmod
18		mov r12, r0
19		mov r8, r12
20		push {r8}
21		pop {r8}
22		mov r8, r8
23		mov r4, r8
24		@ Stack pointer unchanged, no stack allocated arguments
25		ldr r8, =.L.str0
26		push {r8}
27		pop {r8}
28		mov r8, r8
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _prints
32		bl _println
33		@ Stack pointer unchanged, no stack allocated variables
34		mov r0, #0
35		pop {r4, r8, r10, r12}
36		pop {fp, pc}
37	
38	@ length of .L._prints_str0
39		.word 4
40	.L._prints_str0:
41		.asciz "%.*s"
42	.align 4
43	_prints:
44		push {fp, lr}
45		mov fp, sp
46		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
47		bic sp, sp, #0x7
48		mov r2, r0
49		ldr r1, [r0, #-4]
50		adr r0, .L._prints_str0
51		bl printf
52		mov r0, #0
53		bl fflush
54		mov sp, fp
55		pop {fp, pc}
56	
57	@ length of .L._println_str0
58		.word 0
59	.L._println_str0:
60		.asciz ""
61	.align 4
62	_println:
63		push {fp, lr}
64		mov fp, sp
65		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
66		bic sp, sp, #0x7
67		adr r0, .L._println_str0
68		bl puts
69		mov r0, #0
70		bl fflush
71		mov sp, fp
72		pop {fp, pc}
73	
74	_exit:
75		push {fp, lr}
76		mov fp, sp
77		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
78		bic sp, sp, #0x7
79		bl exit
80		mov sp, fp
81		pop {fp, pc}
82	
83	@ length of .L._errDivZero_str0
84		.word 40
85	.L._errDivZero_str0:
86		.asciz "fatal error: division or modulo by zero\n"
87	.align 4
88	_errDivZero:
89		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
90		bic sp, sp, #0x7
91		adr r0, .L._errDivZero_str0
92		bl _prints
93		mov r0, #255
94		bl _exit
===========================================================
-- Finished

