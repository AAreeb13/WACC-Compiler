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
1	// length of .L.str0
2		.word 32
3	.L.str0:
4		.asciz "An escaped character example is "
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		mov fp, sp
12		// Stack pointer unchanged, no stack allocated arguments
13		adrp x8, .L.str0
14		add x8, x8, :lo12:.L.str0
15		// push {x8}
16		stp x8, xzr, [sp, #-16]!
17		// pop {x8}
18		ldp x8, xzr, [sp], #16
19		mov x8, x8
20		mov x0, x8
21		// statement primitives do not return results (but will clobber r0/rax)
22		bl _prints
23		// Stack pointer unchanged, no stack allocated arguments
24		mov x8, #34
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _printc
28		bl _println
29		mov x0, #0
30		// pop {fp, lr}
31		ldp fp, lr, [sp], #16
32		ret
33	
34	// length of .L._prints_str0
35		.word 4
36	.L._prints_str0:
37		.asciz "%.*s"
38	.align 4
39	_prints:
40		// push {lr}
41		stp lr, xzr, [sp, #-16]!
42		mov x2, x0
43		ldrsw x1, [x0, #-4]
44		adr x0, .L._prints_str0
45		bl printf
46		mov x0, #0
47		bl fflush
48		// pop {lr}
49		ldp lr, xzr, [sp], #16
50		ret
51	
52	// length of .L._printc_str0
53		.word 2
54	.L._printc_str0:
55		.asciz "%c"
56	.align 4
57	_printc:
58		// push {lr}
59		stp lr, xzr, [sp, #-16]!
60		mov x1, x0
61		adr x0, .L._printc_str0
62		bl printf
63		mov x0, #0
64		bl fflush
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
68	
69	// length of .L._println_str0
70		.word 0
71	.L._println_str0:
72		.asciz ""
73	.align 4
74	_println:
75		// push {lr}
76		stp lr, xzr, [sp, #-16]!
77		adr x0, .L._println_str0
78		bl puts
79		mov x0, #0
80		bl fflush
81		// pop {lr}
82		ldp lr, xzr, [sp], #16
83		ret
===========================================================
-- Finished

