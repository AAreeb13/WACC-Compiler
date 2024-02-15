./valid/pairs/nestedPair.wacc
calling the reference compiler on ./valid/pairs/nestedPair.wacc
-- Test: nestedPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, pair (int, int) )

# Output:

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPair.s contents are:
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
12		mov r8, #2
13		str r8, [r12, #0]
14		mov r8, #3
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		mov r0, #8
19		bl _malloc
20		mov r12, r0
21		mov r8, #1
22		str r8, [r12, #0]
23		mov r8, r4
24		str r8, [r12, #4]
25		mov r8, r12
26		mov r5, r8
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
51	_malloc:
52		push {fp, lr}
53		mov fp, sp
54		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
55		bic sp, sp, #0x7
56		bl malloc
57		cmp r0, #0
58		bleq _errOutOfMemory
59		mov sp, fp
60		pop {fp, pc}
61	
62	_exit:
63		push {fp, lr}
64		mov fp, sp
65		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
66		bic sp, sp, #0x7
67		bl exit
68		mov sp, fp
69		pop {fp, pc}
70	
71	@ length of .L._errOutOfMemory_str0
72		.word 27
73	.L._errOutOfMemory_str0:
74		.asciz "fatal error: out of memory\n"
75	.align 4
76	_errOutOfMemory:
77		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
78		bic sp, sp, #0x7
79		adr r0, .L._errOutOfMemory_str0
80		bl _prints
81		mov r0, #255
82		bl _exit
===========================================================
-- Finished

