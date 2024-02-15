./valid/IO/print/printEscChar.wacc
calling the reference compiler on ./valid/IO/print/printEscChar.wacc
-- Test: printEscChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic escaped character printing

# Output:
# An escaped character example is "
#

# Program:

begin
  print "An escaped character example is " ;
  println '\"'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printEscChar.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 32
3	.L.str0:
4		.asciz "An escaped character example is "
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated arguments
13		ldr r8, =.L.str0
14		push {r8}
15		pop {r8}
16		mov r8, r8
17		mov r0, r8
18		@ statement primitives do not return results (but will clobber r0/rax)
19		bl _prints
20		@ Stack pointer unchanged, no stack allocated arguments
21		mov r8, #34
22		mov r0, r8
23		@ statement primitives do not return results (but will clobber r0/rax)
24		bl _printc
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
49	@ length of .L._printc_str0
50		.word 2
51	.L._printc_str0:
52		.asciz "%c"
53	.align 4
54	_printc:
55		push {fp, lr}
56		mov fp, sp
57		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
58		bic sp, sp, #0x7
59		mov r1, r0
60		adr r0, .L._printc_str0
61		bl printf
62		mov r0, #0
63		bl fflush
64		mov sp, fp
65		pop {fp, pc}
66	
67	@ length of .L._println_str0
68		.word 0
69	.L._println_str0:
70		.asciz ""
71	.align 4
72	_println:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		adr r0, .L._println_str0
78		bl puts
79		mov r0, #0
80		bl fflush
81		mov sp, fp
82		pop {fp, pc}
===========================================================
-- Finished

