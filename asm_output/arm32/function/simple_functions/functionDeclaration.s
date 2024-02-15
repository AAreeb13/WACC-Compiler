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
5		push {fp, lr}
6		push {r8, r10, r12}
7		mov fp, sp
8		mov r0, #0
9		pop {r8, r10, r12}
10		pop {fp, pc}
11	
12	wacc_f:
13		push {fp, lr}
14		mov fp, sp
15		mov r0, #0
16		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
17		mov sp, fp
18		pop {fp, pc}
19		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
20		@ the .ltorg here is used to generate the constant pool after every function
21		@ this is important for ensuring string literals and large integers are accessible!
22		.ltorg
===========================================================
-- Finished

