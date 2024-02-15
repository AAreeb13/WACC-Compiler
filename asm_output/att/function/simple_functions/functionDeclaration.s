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
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		movq $0, %rax
8		popq %rbx
9		popq %rbp
10		ret
11	
12	wacc_f:
13		pushq %rbp
14		movq %rsp, %rbp
15		movq $0, %rax
16		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
17		movq %rbp, %rsp
18		popq %rbp
19		ret
20		# 'ere be dragons: this is 100% dead code, functions always end in returns!
===========================================================
-- Finished

