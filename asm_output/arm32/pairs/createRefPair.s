./valid/pairs/createRefPair.wacc
calling the reference compiler on ./valid/pairs/createRefPair.wacc
-- Test: createRefPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair(int, char) with a second reference to it

# Output:

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  pair(int, char) q = p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createRefPair.s contents are:
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
9		mov r0, #8
10		bl _malloc
11		mov r12, r0
12		mov r8, #10
13		str r8, [r12, #0]
14		mov r8, #97
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		mov r8, r4
19		mov r5, r8
20		@ Stack pointer unchanged, no stack allocated variables
21		mov r0, #0
22		pop {r4, r5, r8, r10, r12}
23		pop {fp, pc}
24	
25	@ length of .L._prints_str0
26		.word 4
27	.L._prints_str0:
28		.asciz "%.*s"
29	.align 4
30	_prints:
31		push {fp, lr}
32		mov fp, sp
33		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
34		bic sp, sp, #0x7
35		mov r2, r0
36		ldr r1, [r0, #-4]
37		adr r0, .L._prints_str0
38		bl printf
39		mov r0, #0
40		bl fflush
41		mov sp, fp
42		pop {fp, pc}
43	
44	_malloc:
45		push {fp, lr}
46		mov fp, sp
47		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
48		bic sp, sp, #0x7
49		bl malloc
50		cmp r0, #0
51		bleq _errOutOfMemory
52		mov sp, fp
53		pop {fp, pc}
54	
55	_exit:
56		push {fp, lr}
57		mov fp, sp
58		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
59		bic sp, sp, #0x7
60		bl exit
61		mov sp, fp
62		pop {fp, pc}
63	
64	@ length of .L._errOutOfMemory_str0
65		.word 27
66	.L._errOutOfMemory_str0:
67		.asciz "fatal error: out of memory\n"
68	.align 4
69	_errOutOfMemory:
70		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
71		bic sp, sp, #0x7
72		adr r0, .L._errOutOfMemory_str0
73		bl _prints
74		mov r0, #255
75		bl _exit
===========================================================
-- Finished

