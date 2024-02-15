./valid/array/emptyArrayScope.wacc
calling the reference compiler on ./valid/array/emptyArrayScope.wacc
-- Test: emptyArrayScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Scoping shouldn't affect arrays

# Output:

# Program:

begin
  int[] x = [] ;
  begin
    bool y = true
  end
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayScope.s contents are:
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
20		mov r8, #1
21		mov r5, r8
22		@ Stack pointer unchanged, no stack allocated variables
23		@ Stack pointer unchanged, no stack allocated variables
24		mov r0, #0
25		pop {r4, r5, r8, r10, r12}
26		pop {fp, pc}
27	
28	@ length of .L._prints_str0
29		.word 4
30	.L._prints_str0:
31		.asciz "%.*s"
32	.align 4
33	_prints:
34		push {fp, lr}
35		mov fp, sp
36		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
37		bic sp, sp, #0x7
38		mov r2, r0
39		ldr r1, [r0, #-4]
40		adr r0, .L._prints_str0
41		bl printf
42		mov r0, #0
43		bl fflush
44		mov sp, fp
45		pop {fp, pc}
46	
47	_malloc:
48		push {fp, lr}
49		mov fp, sp
50		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
51		bic sp, sp, #0x7
52		bl malloc
53		cmp r0, #0
54		bleq _errOutOfMemory
55		mov sp, fp
56		pop {fp, pc}
57	
58	_exit:
59		push {fp, lr}
60		mov fp, sp
61		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
62		bic sp, sp, #0x7
63		bl exit
64		mov sp, fp
65		pop {fp, pc}
66	
67	@ length of .L._errOutOfMemory_str0
68		.word 27
69	.L._errOutOfMemory_str0:
70		.asciz "fatal error: out of memory\n"
71	.align 4
72	_errOutOfMemory:
73		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
74		bic sp, sp, #0x7
75		adr r0, .L._errOutOfMemory_str0
76		bl _prints
77		mov r0, #255
78		bl _exit
===========================================================
-- Finished

