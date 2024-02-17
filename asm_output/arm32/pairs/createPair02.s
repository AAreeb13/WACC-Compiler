./valid/pairs/createPair02.wacc
calling the reference compiler on ./valid/pairs/createPair02.wacc
-- Test: createPair02.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (char, char)

# Output:

# Program:

begin
  pair(char, char) p = newpair('a', 'b')
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createPair02.s contents are:
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
9		mov r0, #8
10		bl _malloc
11		mov r12, r0
12		mov r8, #97
13		str r8, [r12, #0]
14		mov r8, #98
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		@ Stack pointer unchanged, no stack allocated variables
19		mov r0, #0
20		pop {r4, r8, r10, r12}
21		pop {fp, pc}
22	
23	@ length of .L._prints_str0
24		.word 4
25	.L._prints_str0:
26		.asciz "%.*s"
27	.align 4
28	_prints:
29		push {fp, lr}
30		mov fp, sp
31		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
32		bic sp, sp, #0x7
33		mov r2, r0
34		ldr r1, [r0, #-4]
35		adr r0, .L._prints_str0
36		bl printf
37		mov r0, #0
38		bl fflush
39		mov sp, fp
40		pop {fp, pc}
41	
42	_malloc:
43		push {fp, lr}
44		mov fp, sp
45		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
46		bic sp, sp, #0x7
47		bl malloc
48		cmp r0, #0
49		bleq _errOutOfMemory
50		mov sp, fp
51		pop {fp, pc}
52	
53	_exit:
54		push {fp, lr}
55		mov fp, sp
56		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
57		bic sp, sp, #0x7
58		bl exit
59		mov sp, fp
60		pop {fp, pc}
61	
62	@ length of .L._errOutOfMemory_str0
63		.word 27
64	.L._errOutOfMemory_str0:
65		.asciz "fatal error: out of memory\n"
66	.align 4
67	_errOutOfMemory:
68		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
69		bic sp, sp, #0x7
70		adr r0, .L._errOutOfMemory_str0
71		bl _prints
72		mov r0, #255
73		bl _exit
===========================================================
-- Finished

