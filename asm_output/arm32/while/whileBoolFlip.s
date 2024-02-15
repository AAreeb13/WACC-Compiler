./valid/while/whileBoolFlip.wacc
calling the reference compiler on ./valid/while/whileBoolFlip.wacc
-- Test: whileBoolFlip.wacc

-- Uploaded file: 
---------------------------------------------------------------
# while loop flips bool to terminate

# Output:
# flip b!
# end of loop
#

# Program:

begin
  bool b = true ;
  while b do
    println "flip b!" ;
    b = !b
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileBoolFlip.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 7
3	.L.str0:
4		.asciz "flip b!"
5	@ length of .L.str1
6		.word 11
7	.L.str1:
8		.asciz "end of loop"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #1
18		mov r4, r8
19		b .L0
20	.L1:
21		@ Stack pointer unchanged, no stack allocated arguments
22		ldr r8, =.L.str0
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _prints
29		bl _println
30		cmp r4, #1
31		movne r8, #1
32		moveq r8, #0
33		push {r8}
34		pop {r8}
35		mov r8, r8
36		mov r4, r8
37	.L0:
38		cmp r4, #1
39		beq .L1
40		@ Stack pointer unchanged, no stack allocated arguments
41		ldr r8, =.L.str1
42		push {r8}
43		pop {r8}
44		mov r8, r8
45		mov r0, r8
46		@ statement primitives do not return results (but will clobber r0/rax)
47		bl _prints
48		bl _println
49		@ Stack pointer unchanged, no stack allocated variables
50		mov r0, #0
51		pop {r4, r8, r10, r12}
52		pop {fp, pc}
53	
54	@ length of .L._prints_str0
55		.word 4
56	.L._prints_str0:
57		.asciz "%.*s"
58	.align 4
59	_prints:
60		push {fp, lr}
61		mov fp, sp
62		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
63		bic sp, sp, #0x7
64		mov r2, r0
65		ldr r1, [r0, #-4]
66		adr r0, .L._prints_str0
67		bl printf
68		mov r0, #0
69		bl fflush
70		mov sp, fp
71		pop {fp, pc}
72	
73	@ length of .L._println_str0
74		.word 0
75	.L._println_str0:
76		.asciz ""
77	.align 4
78	_println:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		adr r0, .L._println_str0
84		bl puts
85		mov r0, #0
86		bl fflush
87		mov sp, fp
88		pop {fp, pc}
===========================================================
-- Finished

