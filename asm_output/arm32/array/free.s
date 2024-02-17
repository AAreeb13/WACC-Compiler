./valid/array/free.wacc
calling the reference compiler on ./valid/array/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free an array

# Output:

# Program:

begin
  int[] arr = [1, 2, 3] ;
  free arr
end


---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
9		@ 3 element array
10		mov r0, #16
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #3
16		str r8, [r12, #-4]
17		mov r8, #1
18		str r8, [r12, #0]
19		mov r8, #2
20		str r8, [r12, #4]
21		mov r8, #3
22		str r8, [r12, #8]
23		mov r8, r12
24		mov r4, r8
25		@ Stack pointer unchanged, no stack allocated arguments
26		@ array pointers are shifted forward by 4 bytes, so correct it back to original pointer before free
27		sub r8, r4, #4
28		push {r8}
29		pop {r8}
30		mov r8, r8
31		mov r0, r8
32		@ statement primitives do not return results (but will clobber r0/rax)
33		bl _free
34		@ Stack pointer unchanged, no stack allocated variables
35		mov r0, #0
36		pop {r4, r8, r10, r12}
37		pop {fp, pc}
38	
39	@ length of .L._prints_str0
40		.word 4
41	.L._prints_str0:
42		.asciz "%.*s"
43	.align 4
44	_prints:
45		push {fp, lr}
46		mov fp, sp
47		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
48		bic sp, sp, #0x7
49		mov r2, r0
50		ldr r1, [r0, #-4]
51		adr r0, .L._prints_str0
52		bl printf
53		mov r0, #0
54		bl fflush
55		mov sp, fp
56		pop {fp, pc}
57	
58	_malloc:
59		push {fp, lr}
60		mov fp, sp
61		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
62		bic sp, sp, #0x7
63		bl malloc
64		cmp r0, #0
65		bleq _errOutOfMemory
66		mov sp, fp
67		pop {fp, pc}
68	
69	_free:
70		push {fp, lr}
71		mov fp, sp
72		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
73		bic sp, sp, #0x7
74		bl free
75		mov sp, fp
76		pop {fp, pc}
77	
78	_exit:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		bl exit
84		mov sp, fp
85		pop {fp, pc}
86	
87	@ length of .L._errOutOfMemory_str0
88		.word 27
89	.L._errOutOfMemory_str0:
90		.asciz "fatal error: out of memory\n"
91	.align 4
92	_errOutOfMemory:
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		adr r0, .L._errOutOfMemory_str0
96		bl _prints
97		mov r0, #255
98		bl _exit
===========================================================
-- Finished

