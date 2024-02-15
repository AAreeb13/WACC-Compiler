./valid/IO/print/print.wacc
calling the reference compiler on ./valid/IO/print/print.wacc
-- Test: print.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple print statement

# Output:
# Hello World!

# Program:

begin
  print "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
print.s contents are:
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
23		mov x0, #0
24		// pop {fp, lr}
25		ldp fp, lr, [sp], #16
26		ret
27	
28	// length of .L._prints_str0
29		.word 4
30	.L._prints_str0:
31		.asciz "%.*s"
32	.align 4
33	_prints:
34		// push {lr}
35		stp lr, xzr, [sp, #-16]!
36		mov x2, x0
37		ldrsw x1, [x0, #-4]
38		adr x0, .L._prints_str0
39		bl printf
40		mov x0, #0
41		bl fflush
42		// pop {lr}
43		ldp lr, xzr, [sp], #16
44		ret
===========================================================
-- Finished

