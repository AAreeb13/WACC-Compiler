./valid/while/whileFalse.wacc
calling the reference compiler on ./valid/while/whileFalse.wacc
-- Test: whileFalse.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple unentered while loop

# Output:
# end of loop
#

# Program:

begin
  while false do
    println "looping..."
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileFalse.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 10
3	.L.str0:
4		.asciz "looping..."
5	@ length of .L.str1
6		.word 11
7	.L.str1:
8		.asciz "end of loop"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r8, r10, r12}
15		mov fp, sp
16		b .L0
17	.L1:
18		@ Stack pointer unchanged, no stack allocated arguments
19		ldr r8, =.L.str0
20		push {r8}
21		pop {r8}
22		mov r8, r8
23		mov r0, r8
24		@ statement primitives do not return results (but will clobber r0/rax)
25		bl _prints
26		bl _println
27	.L0:
28		mov r8, #0
29		cmp r8, #1
30		beq .L1
31		@ Stack pointer unchanged, no stack allocated arguments
32		ldr r8, =.L.str1
33		push {r8}
34		pop {r8}
35		mov r8, r8
36		mov r0, r8
37		@ statement primitives do not return results (but will clobber r0/rax)
38		bl _prints
39		bl _println
40		mov r0, #0
41		pop {r8, r10, r12}
42		pop {fp, pc}
43	
44	@ length of .L._prints_str0
45		.word 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.align 4
49	_prints:
50		push {fp, lr}
51		mov fp, sp
52		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
53		bic sp, sp, #0x7
54		mov r2, r0
55		ldr r1, [r0, #-4]
56		adr r0, .L._prints_str0
57		bl printf
58		mov r0, #0
59		bl fflush
60		mov sp, fp
61		pop {fp, pc}
62	
63	@ length of .L._println_str0
64		.word 0
65	.L._println_str0:
66		.asciz ""
67	.align 4
68	_println:
69		push {fp, lr}
70		mov fp, sp
71		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
72		bic sp, sp, #0x7
73		adr r0, .L._println_str0
74		bl puts
75		mov r0, #0
76		bl fflush
77		mov sp, fp
78		pop {fp, pc}
===========================================================
-- Finished

