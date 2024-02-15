./valid/pairs/printNull.wacc
calling the reference compiler on ./valid/pairs/printNull.wacc
-- Test: printNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the null reference

# Output:
# (nil)
#

# Program:

begin
  println null
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printNull.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		// Stack pointer unchanged, no stack allocated arguments
9		mov x8, #0
10		mov x0, x8
11		// statement primitives do not return results (but will clobber r0/rax)
12		bl _printp
13		bl _println
14		mov x0, #0
15		// pop {fp, lr}
16		ldp fp, lr, [sp], #16
17		ret
18	
19	// length of .L._printp_str0
20		.word 2
21	.L._printp_str0:
22		.asciz "%p"
23	.align 4
24	_printp:
25		// push {lr}
26		stp lr, xzr, [sp, #-16]!
27		mov x1, x0
28		adr x0, .L._printp_str0
29		bl printf
30		mov x0, #0
31		bl fflush
32		// pop {lr}
33		ldp lr, xzr, [sp], #16
34		ret
35	
36	// length of .L._println_str0
37		.word 0
38	.L._println_str0:
39		.asciz ""
40	.align 4
41	_println:
42		// push {lr}
43		stp lr, xzr, [sp, #-16]!
44		adr x0, .L._println_str0
45		bl puts
46		mov x0, #0
47		bl fflush
48		// pop {lr}
49		ldp lr, xzr, [sp], #16
50		ret
===========================================================
-- Finished

