./valid/sequence/exitSimple.wacc
calling the reference compiler on ./valid/sequence/exitSimple.wacc
-- Test: exitSimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# exit with unreachable print

# Output:

# Exit:
# 42

# Program:

begin
  exit 42 ;
  println "Should not print this."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exitSimple.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 22
3	.L.str0:
4		.asciz "Should not print this."
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated arguments
13		mov r8, #42
14		mov r0, r8
15		@ statement primitives do not return results (but will clobber r0/rax)
16		bl _exit
17		@ Stack pointer unchanged, no stack allocated arguments
18		ldr r8, =.L.str0
19		push {r8}
20		pop {r8}
21		mov r8, r8
22		mov r0, r8
23		@ statement primitives do not return results (but will clobber r0/rax)
24		bl _prints
25		bl _println
26		mov r0, #0
27		pop {r8, r10, r12}
28		pop {fp, pc}
29	
30	@ length of .L._prints_str0
31		.word 4
32	.L._prints_str0:
33		.asciz "%.*s"
34	.align 4
35	_prints:
36		push {fp, lr}
37		mov fp, sp
38		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
39		bic sp, sp, #0x7
40		mov r2, r0
41		ldr r1, [r0, #-4]
42		adr r0, .L._prints_str0
43		bl printf
44		mov r0, #0
45		bl fflush
46		mov sp, fp
47		pop {fp, pc}
48	
49	@ length of .L._println_str0
50		.word 0
51	.L._println_str0:
52		.asciz ""
53	.align 4
54	_println:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		adr r0, .L._println_str0
60		bl puts
61		mov r0, #0
62		bl fflush
63		mov sp, fp
64		pop {fp, pc}
65	
66	_exit:
67		push {fp, lr}
68		mov fp, sp
69		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
70		bic sp, sp, #0x7
71		bl exit
72		mov sp, fp
73		pop {fp, pc}
===========================================================
-- Finished

