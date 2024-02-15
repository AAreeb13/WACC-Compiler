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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		mov rax, 0
9		pop rbx
10		pop rbp
11		ret
12	
13	wacc_f:
14		push rbp
15		mov rbp, rsp
16		mov rax, 0
17		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
18		mov rsp, rbp
19		pop rbp
20		ret
21		# 'ere be dragons: this is 100% dead code, functions always end in returns!
===========================================================
-- Finished

