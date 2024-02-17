./valid/runtimeErr/nullDereference/setNull2.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/setNull2.wacc
-- Test: setNull2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by setting an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	snd p = 1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
setNull2.s contents are:
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
11		cmp r4, #0
12		bleq _errNull
13		mov r8, #1
14		str r8, [r4, #4]
15		@ Stack pointer unchanged, no stack allocated variables
16		mov r0, #0
17		pop {r4, r8, r10, r12}
18		pop {fp, pc}
19	
20	@ length of .L._prints_str0
21		.word 4
22	.L._prints_str0:
23		.asciz "%.*s"
24	.align 4
25	_prints:
26		push {fp, lr}
27		mov fp, sp
28		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
29		bic sp, sp, #0x7
30		mov r2, r0
31		ldr r1, [r0, #-4]
32		adr r0, .L._prints_str0
33		bl printf
34		mov r0, #0
35		bl fflush
36		mov sp, fp
37		pop {fp, pc}
38	
39	_exit:
40		push {fp, lr}
41		mov fp, sp
42		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
43		bic sp, sp, #0x7
44		bl exit
45		mov sp, fp
46		pop {fp, pc}
47	
48	@ length of .L._errNull_str0
49		.word 45
50	.L._errNull_str0:
51		.asciz "fatal error: null pair dereferenced or freed\n"
52	.align 4
53	_errNull:
54		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
55		bic sp, sp, #0x7
56		adr r0, .L._errNull_str0
57		bl _prints
58		mov r0, #255
59		bl _exit
===========================================================
-- Finished

