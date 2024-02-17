./valid/if/if6.wacc
calling the reference compiler on ./valid/if/if6.wacc
-- Test: if6.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Simple conditional statement with character comparison test

# Output:
# correct
#

# Program:

begin
  char c1 = 'f';
  char c2 = 'F';
  if c1 == c2
  then
    println "incorrect"
  else
    println "correct"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
if6.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 7
3	.L.str0:
4		.asciz "correct"
5	@ length of .L.str1
6		.word 9
7	.L.str1:
8		.asciz "incorrect"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r5, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #102
18		mov r4, r8
19		mov r8, #70
20		mov r5, r8
21		cmp r4, r5
22		beq .L0
23		@ Stack pointer unchanged, no stack allocated arguments
24		ldr r8, =.L.str0
25		push {r8}
26		pop {r8}
27		mov r8, r8
28		mov r0, r8
29		@ statement primitives do not return results (but will clobber r0/rax)
30		bl _prints
31		bl _println
32		b .L1
33	.L0:
34		@ Stack pointer unchanged, no stack allocated arguments
35		ldr r8, =.L.str1
36		push {r8}
37		pop {r8}
38		mov r8, r8
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		bl _println
43	.L1:
44		@ Stack pointer unchanged, no stack allocated variables
45		mov r0, #0
46		pop {r4, r5, r8, r10, r12}
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

