./valid/IO/print/println.wacc
calling the reference compiler on ./valid/IO/print/println.wacc
-- Test: println.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple println statement

# Output:
# Hello World!
#

# Program:

begin
  println "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
println.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "Hello World!"
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
23		bl _println
24		mov x0, #0
25		// pop {fp, lr}
26		ldp fp, lr, [sp], #16
27		ret
28	
29	// length of .L._prints_str0
30		.word 4
31	.L._prints_str0:
32		.asciz "%.*s"
33	.align 4
34	_prints:
35		// push {lr}
36		stp lr, xzr, [sp, #-16]!
37		mov x2, x0
38		ldrsw x1, [x0, #-4]
39		adr x0, .L._prints_str0
40		bl printf
41		mov x0, #0
42		bl fflush
43		// pop {lr}
44		ldp lr, xzr, [sp], #16
45		ret
46	
47	// length of .L._println_str0
48		.word 0
49	.L._println_str0:
50		.asciz ""
51	.align 4
52	_println:
53		// push {lr}
54		stp lr, xzr, [sp, #-16]!
55		adr x0, .L._println_str0
56		bl puts
57		mov x0, #0
58		bl fflush
59		// pop {lr}
60		ldp lr, xzr, [sp], #16
61		ret
===========================================================
-- Finished

