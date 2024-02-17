./valid/runtimeErr/nullDereference/useNull2.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/useNull2.wacc
-- Test: useNull2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by using it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	int x = snd p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
useNull2.s contents are:
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
9		mov r8, #0
10		mov r4, r8
11		cmp r4, #0
12		bleq _errNull
13		ldr r8, [r4, #4]
14		mov r8, r8
15		mov r5, r8
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r0, #0
18		pop {r4, r5, r8, r10, r12}
19		pop {fp, pc}
20	
21	@ length of .L._prints_str0
22		.word 4
23	.L._prints_str0:
24		.asciz "%.*s"
25	.align 4
26	_prints:
27		push {fp, lr}
28		mov fp, sp
29		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
30		bic sp, sp, #0x7
31		mov r2, r0
32		ldr r1, [r0, #-4]
33		adr r0, .L._prints_str0
34		bl printf
35		mov r0, #0
36		bl fflush
37		mov sp, fp
38		pop {fp, pc}
39	
40	_exit:
41		push {fp, lr}
42		mov fp, sp
43		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
44		bic sp, sp, #0x7
45		bl exit
46		mov sp, fp
47		pop {fp, pc}
48	
49	@ length of .L._errNull_str0
50		.word 45
51	.L._errNull_str0:
52		.asciz "fatal error: null pair dereferenced or freed\n"
53	.align 4
54	_errNull:
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		adr r0, .L._errNull_str0
58		bl _prints
59		mov r0, #255
60		bl _exit
===========================================================
-- Finished

