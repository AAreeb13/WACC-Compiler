./valid/function/simple_functions/sameNameAsVar.wacc
calling the reference compiler on ./valid/function/simple_functions/sameNameAsVar.wacc
-- Test: sameNameAsVar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# program with function that has same name as a variable

# Output:
# 5
#

# Program:

begin
  int f() is
    return 5
  end

  int f = call f();
  println f
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sameNameAsVar.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// Stack pointer unchanged, no stack allocated arguments
12		bl wacc_f
13		mov x16, x0
14		// Stack pointer unchanged, no stack allocated arguments
15		mov x8, x16
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		mov x8, x19
19		mov x0, x8
20		// statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		// Stack pointer unchanged, no stack allocated variables
24		mov x0, #0
25		// pop {x19}
26		ldp x19, xzr, [sp], #16
27		// pop {fp, lr}
28		ldp fp, lr, [sp], #16
29		ret
30	
31	wacc_f:
32		// push {fp, lr}
33		stp fp, lr, [sp, #-16]!
34		mov fp, sp
35		mov x0, #5
36		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
37		mov sp, fp
38		// pop {fp, lr}
39		ldp fp, lr, [sp], #16
40		ret
41		// 'ere be dragons: this is 100% dead code, functions always end in returns!
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

