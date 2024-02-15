./valid/runtimeErr/divideByZero/divideByZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divideByZero.wacc
-- Test: divideByZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt divide by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	int x = 10 ;
	int y = 0 ;
	print x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divideByZero.s contents are:
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
9		mov r8, #10
10		mov r4, r8
11		mov r8, #0
12		mov r5, r8
13		@ Stack pointer unchanged, no stack allocated arguments
14		mov r0, r4
15		mov r1, r5
16		cmp r1, #0
17		bleq _errDivZero
18		bl __aeabi_idivmod
19		mov r12, r0
20		mov r8, r12
21		push {r8}
22		pop {r8}
23		mov r8, r8
24		mov r0, r8
25		@ statement primitives do not return results (but will clobber r0/rax)
26		bl _printi
27		@ Stack pointer unchanged, no stack allocated variables
28		mov r0, #0
29		pop {r4, r5, r8, r10, r12}
30		pop {fp, pc}
31	
32	@ length of .L._prints_str0
33		.word 4
34	.L._prints_str0:
35		.asciz "%.*s"
36	.align 4
37	_prints:
38		push {fp, lr}
39		mov fp, sp
40		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
41		bic sp, sp, #0x7
42		mov r2, r0
43		ldr r1, [r0, #-4]
44		adr r0, .L._prints_str0
45		bl printf
46		mov r0, #0
47		bl fflush
48		mov sp, fp
49		pop {fp, pc}
50	
51	@ length of .L._printi_str0
52		.word 2
53	.L._printi_str0:
54		.asciz "%d"
55	.align 4
56	_printi:
57		push {fp, lr}
58		mov fp, sp
59		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
60		bic sp, sp, #0x7
61		mov r1, r0
62		adr r0, .L._printi_str0
63		bl printf
64		mov r0, #0
65		bl fflush
66		mov sp, fp
67		pop {fp, pc}
68	
69	_exit:
70		push {fp, lr}
71		mov fp, sp
72		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
73		bic sp, sp, #0x7
74		bl exit
75		mov sp, fp
76		pop {fp, pc}
77	
78	@ length of .L._errDivZero_str0
79		.word 40
80	.L._errDivZero_str0:
81		.asciz "fatal error: division or modulo by zero\n"
82	.align 4
83	_errDivZero:
84		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
85		bic sp, sp, #0x7
86		adr r0, .L._errDivZero_str0
87		bl _prints
88		mov r0, #255
89		bl _exit
===========================================================
-- Finished

