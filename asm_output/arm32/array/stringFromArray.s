./valid/array/stringFromArray.wacc
calling the reference compiler on ./valid/array/stringFromArray.wacc
-- Test: stringFromArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# char[] can be directly assigned to string

# Output:

# Program:

begin
  string a = ['a', 'b'] ;
  exit 0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringFromArray.s contents are:
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
9		@ 2 element array
10		mov r0, #6
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #2
16		str r8, [r12, #-4]
17		mov r8, #97
18		strb r8, [r12, #0]
19		mov r8, #98
20		strb r8, [r12, #1]
21		mov r8, r12
22		mov r4, r8
23		@ Stack pointer unchanged, no stack allocated arguments
24		mov r8, #0
25		mov r0, r8
26		@ statement primitives do not return results (but will clobber r0/rax)
27		bl _exit
28		@ Stack pointer unchanged, no stack allocated variables
29		mov r0, #0
30		pop {r4, r8, r10, r12}
31		pop {fp, pc}
32	
33	@ length of .L._prints_str0
34		.word 4
35	.L._prints_str0:
36		.asciz "%.*s"
37	.align 4
38	_prints:
39		push {fp, lr}
40		mov fp, sp
41		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
42		bic sp, sp, #0x7
43		mov r2, r0
44		ldr r1, [r0, #-4]
45		adr r0, .L._prints_str0
46		bl printf
47		mov r0, #0
48		bl fflush
49		mov sp, fp
50		pop {fp, pc}
51	
52	_malloc:
53		push {fp, lr}
54		mov fp, sp
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		bl malloc
58		cmp r0, #0
59		bleq _errOutOfMemory
60		mov sp, fp
61		pop {fp, pc}
62	
63	_exit:
64		push {fp, lr}
65		mov fp, sp
66		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
67		bic sp, sp, #0x7
68		bl exit
69		mov sp, fp
70		pop {fp, pc}
71	
72	@ length of .L._errOutOfMemory_str0
73		.word 27
74	.L._errOutOfMemory_str0:
75		.asciz "fatal error: out of memory\n"
76	.align 4
77	_errOutOfMemory:
78		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
79		bic sp, sp, #0x7
80		adr r0, .L._errOutOfMemory_str0
81		bl _prints
82		mov r0, #255
83		bl _exit
===========================================================
-- Finished

