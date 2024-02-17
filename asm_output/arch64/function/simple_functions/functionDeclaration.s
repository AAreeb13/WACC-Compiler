./valid/function/simple_functions/functionDeclaration.wacc
calling the reference compiler on ./valid/function/simple_functions/functionDeclaration.wacc
-- Test: functionDeclaration.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function is declared, but not called

# Output:

# Program:

begin
  int f() is
    return 0
  end
  skip
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionDeclaration.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		mov x0, #0
9		// pop {fp, lr}
10		ldp fp, lr, [sp], #16
11		ret
12	
13	wacc_f:
14		// push {fp, lr}
15		stp fp, lr, [sp, #-16]!
16		mov fp, sp
17		mov x0, #0
18		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
19		mov sp, fp
20		// pop {fp, lr}
21		ldp fp, lr, [sp], #16
22		ret
23		// 'ere be dragons: this is 100% dead code, functions always end in returns!
===========================================================
-- Finished

