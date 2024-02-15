./valid/while/loopCharCondition.wacc
calling the reference compiler on ./valid/while/loopCharCondition.wacc
-- Test: loopCharCondition.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Use a character as a loop condition. Enter the loop once only, then exit the loop.

# Output:
# Change c
# Should print "Change c" once before.
#

# Program:

begin
	char c = '\0' ;
	while c == '\0' do
		c = 'a' ;
		println "Change c"
	done ;
	println "Should print \"Change c\" once before."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
loopCharCondition.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "Change c"
5	@ length of .L.str1
6		.word 36
7	.L.str1:
8		.asciz "Should print \"Change c\" once before."
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #0
18		mov r4, r8
19		b .L0
20	.L1:
21		mov r8, #97
22		mov r4, r8
23		@ Stack pointer unchanged, no stack allocated arguments
24		ldr r8, =.L.str0
25		push {r8}
26		pop {r8}
27		mov r8, r8
28		mov r0, r8
29		@ statement primitives do not return results (but will clobber r0/rax)
30		bl _prints
31		bl _println
32	.L0:
33		cmp r4, #0
34		beq .L1
35		@ Stack pointer unchanged, no stack allocated arguments
36		ldr r8, =.L.str1
37		push {r8}
38		pop {r8}
39		mov r8, r8
40		mov r0, r8
41		@ statement primitives do not return results (but will clobber r0/rax)
42		bl _prints
43		bl _println
44		@ Stack pointer unchanged, no stack allocated variables
45		mov r0, #0
46		pop {r4, r8, r10, r12}
47		pop {fp, pc}
48	
49	@ length of .L._prints_str0
50		.word 4
51	.L._prints_str0:
52		.asciz "%.*s"
53	.align 4
54	_prints:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		mov r2, r0
60		ldr r1, [r0, #-4]
61		adr r0, .L._prints_str0
62		bl printf
63		mov r0, #0
64		bl fflush
65		mov sp, fp
66		pop {fp, pc}
67	
68	@ length of .L._println_str0
69		.word 0
70	.L._println_str0:
71		.asciz ""
72	.align 4
73	_println:
74		push {fp, lr}
75		mov fp, sp
76		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
77		bic sp, sp, #0x7
78		adr r0, .L._println_str0
79		bl puts
80		mov r0, #0
81		bl fflush
82		mov sp, fp
83		pop {fp, pc}
===========================================================
-- Finished

