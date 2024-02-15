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
1	// length of .L.str0
2		.word 22
3	.L.str0:
4		.asciz "Should not print this."
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		mov fp, sp
12		// Stack pointer unchanged, no stack allocated arguments
13		mov x8, #42
14		mov x0, x8
15		// statement primitives do not return results (but will clobber r0/rax)
16		bl exit
17		// Stack pointer unchanged, no stack allocated arguments
18		adrp x8, .L.str0
19		add x8, x8, :lo12:.L.str0
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _prints
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
52	// length of .L._println_str0
53		.word 0
54	.L._println_str0:
55		.asciz ""
56	.align 4
57	_println:
58		// push {lr}
59		stp lr, xzr, [sp, #-16]!
60		adr x0, .L._println_str0
61		bl puts
62		mov x0, #0
63		bl fflush
64		// pop {lr}
65		ldp lr, xzr, [sp], #16
66		ret
===========================================================
-- Finished

