./valid/IO/print/printCharArray.wacc
calling the reference compiler on ./valid/IO/print/printCharArray.wacc
-- Test: printCharArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# printing the contents of a char[] is possible via an intermediate variable

# Output:
# hi!
#

# Exit:
# 0

# Program:

begin
  char[] s = ['h','i','!'];
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharArray.s contents are:
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
10		mov r0, #7
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #3
16		str r8, [r12, #-4]
17		mov r8, #104
18		strb r8, [r12, #0]
19		mov r8, #105
20		strb r8, [r12, #1]
21		mov r8, #33
22		strb r8, [r12, #2]
23		mov r8, r12
24		mov r4, r8
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r4
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _prints
30		bl _println
31		@ Stack pointer unchanged, no stack allocated variables
32		mov r0, #0
33		pop {r4, r8, r10, r12}
34		pop {fp, pc}
35	
36	@ length of .L._prints_str0
37		.word 4
38	.L._prints_str0:
39		.asciz "%.*s"
40	.align 4
41	_prints:
42		push {fp, lr}
43		mov fp, sp
44		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
45		bic sp, sp, #0x7
46		mov r2, r0
47		ldr r1, [r0, #-4]
48		adr r0, .L._prints_str0
49		bl printf
50		mov r0, #0
51		bl fflush
52		mov sp, fp
53		pop {fp, pc}
54	
55	_malloc:
56		push {fp, lr}
57		mov fp, sp
58		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
59		bic sp, sp, #0x7
60		bl malloc
61		cmp r0, #0
62		bleq _errOutOfMemory
63		mov sp, fp
64		pop {fp, pc}
65	
66	@ length of .L._println_str0
67		.word 0
68	.L._println_str0:
69		.asciz ""
70	.align 4
71	_println:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		adr r0, .L._println_str0
77		bl puts
78		mov r0, #0
79		bl fflush
80		mov sp, fp
81		pop {fp, pc}
82	
83	_exit:
84		push {fp, lr}
85		mov fp, sp
86		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
87		bic sp, sp, #0x7
88		bl exit
89		mov sp, fp
90		pop {fp, pc}
91	
92	@ length of .L._errOutOfMemory_str0
93		.word 27
94	.L._errOutOfMemory_str0:
95		.asciz "fatal error: out of memory\n"
96	.align 4
97	_errOutOfMemory:
98		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
99		bic sp, sp, #0x7
100		adr r0, .L._errOutOfMemory_str0
101		bl _prints
102		mov r0, #255
103		bl _exit
===========================================================
-- Finished

