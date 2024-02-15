./valid/array/emptyArrayAloneIsFine.wacc
calling the reference compiler on ./valid/array/emptyArrayAloneIsFine.wacc
-- Test: emptyArrayAloneIsFine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# this is fine

# Output:

# Program:

begin
  int[] x = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayAloneIsFine.s contents are:
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
9		@ 0 element array
10		mov r0, #4
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #0
16		str r8, [r12, #-4]
17		mov r8, r12
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated variables
20		mov r0, #0
21		pop {r4, r8, r10, r12}
22		pop {fp, pc}
23	
24	@ length of .L._prints_str0
25		.word 4
26	.L._prints_str0:
27		.asciz "%.*s"
28	.align 4
29	_prints:
30		push {fp, lr}
31		mov fp, sp
32		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
33		bic sp, sp, #0x7
34		mov r2, r0
35		ldr r1, [r0, #-4]
36		adr r0, .L._prints_str0
37		bl printf
38		mov r0, #0
39		bl fflush
40		mov sp, fp
41		pop {fp, pc}
42	
43	_malloc:
44		push {fp, lr}
45		mov fp, sp
46		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
47		bic sp, sp, #0x7
48		bl malloc
49		cmp r0, #0
50		bleq _errOutOfMemory
51		mov sp, fp
52		pop {fp, pc}
53	
54	_exit:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		bl exit
60		mov sp, fp
61		pop {fp, pc}
62	
63	@ length of .L._errOutOfMemory_str0
64		.word 27
65	.L._errOutOfMemory_str0:
66		.asciz "fatal error: out of memory\n"
67	.align 4
68	_errOutOfMemory:
69		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
70		bic sp, sp, #0x7
71		adr r0, .L._errOutOfMemory_str0
72		bl _prints
73		mov r0, #255
74		bl _exit
===========================================================
-- Finished

