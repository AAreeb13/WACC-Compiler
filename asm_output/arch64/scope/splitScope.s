./valid/scope/splitScope.wacc
calling the reference compiler on ./valid/scope/splitScope.wacc
-- Test: splitScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# splits the first appearances of variables by a new scope to ensure proper grouping

# Output:
# 3
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 3 ;
    bool z = true
  end ;
  int y = 2 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
splitScope.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21}
8		stp x19, x20, [sp, #-32]!
9		stp x21, xzr, [sp, #16]
10		mov fp, sp
11		// Stack pointer unchanged, no stack allocated variables
12		mov x8, #1
13		mov x19, x8
14		// Stack pointer unchanged, no stack allocated variables
15		mov x8, #3
16		mov x19, x8
17		mov x8, #1
18		mov x21, x8
19		// Stack pointer unchanged, no stack allocated variables
20		mov x8, #2
21		mov x20, x8
22		// Stack pointer unchanged, no stack allocated arguments
23		mov x8, x19
24		mov x0, x8
25		// statement primitives do not return results (but will clobber r0/rax)
26		bl _printi
27		bl _println
28		// Stack pointer unchanged, no stack allocated arguments
29		mov x8, x20
30		mov x0, x8
31		// statement primitives do not return results (but will clobber r0/rax)
32		bl _printi
33		bl _println
34		// Stack pointer unchanged, no stack allocated variables
35		mov x0, #0
36		// pop {x19, x20, x21}
37		ldp x21, xzr, [sp, #16]
38		ldp x19, x20, [sp], #32
39		// pop {fp, lr}
40		ldp fp, lr, [sp], #16
41		ret
42	
43	// length of .L._printi_str0
44		.word 2
45	.L._printi_str0:
46		.asciz "%d"
47	.align 4
48	_printi:
49		// push {lr}
50		stp lr, xzr, [sp, #-16]!
51		mov x1, x0
52		adr x0, .L._printi_str0
53		bl printf
54		mov x0, #0
55		bl fflush
56		// pop {lr}
57		ldp lr, xzr, [sp], #16
58		ret
59	
60	// length of .L._println_str0
61		.word 0
62	.L._println_str0:
63		.asciz ""
64	.align 4
65	_println:
66		// push {lr}
67		stp lr, xzr, [sp, #-16]!
68		adr x0, .L._println_str0
69		bl puts
70		mov x0, #0
71		bl fflush
72		// pop {lr}
73		ldp lr, xzr, [sp], #16
74		ret
===========================================================
-- Finished

