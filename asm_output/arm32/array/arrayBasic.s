./valid/array/arrayBasic.wacc
calling the reference compiler on ./valid/array/arrayBasic.wacc
-- Test: arrayBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:

# Program:

begin
  int[] a = [0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayBasic.s contents are:
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
9		@ 1 element array
10		mov r0, #8
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #1
16		str r8, [r12, #-4]
17		mov r8, #0
18		str r8, [r12, #0]
19		mov r8, r12
20		mov r4, r8
21		@ Stack pointer unchanged, no stack allocated variables
22		mov r0, #0
23		pop {r4, r8, r10, r12}
24		pop {fp, pc}
25	
26	@ length of .L._prints_str0
27		.word 4
28	.L._prints_str0:
29		.asciz "%.*s"
30	.align 4
31	_prints:
32		push {fp, lr}
33		mov fp, sp
34		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
35		bic sp, sp, #0x7
36		mov r2, r0
37		ldr r1, [r0, #-4]
38		adr r0, .L._prints_str0
39		bl printf
40		mov r0, #0
41		bl fflush
42		mov sp, fp
43		pop {fp, pc}
44	
45	_malloc:
46		push {fp, lr}
47		mov fp, sp
48		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
49		bic sp, sp, #0x7
50		bl malloc
51		cmp r0, #0
52		bleq _errOutOfMemory
53		mov sp, fp
54		pop {fp, pc}
55	
56	_exit:
57		push {fp, lr}
58		mov fp, sp
59		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
60		bic sp, sp, #0x7
61		bl exit
62		mov sp, fp
63		pop {fp, pc}
64	
65	@ length of .L._errOutOfMemory_str0
66		.word 27
67	.L._errOutOfMemory_str0:
68		.asciz "fatal error: out of memory\n"
69	.align 4
70	_errOutOfMemory:
71		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
72		bic sp, sp, #0x7
73		adr r0, .L._errOutOfMemory_str0
74		bl _prints
75		mov r0, #255
76		bl _exit
===========================================================
-- Finished

