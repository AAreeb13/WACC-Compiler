./valid/scope/scopeBasic.wacc
calling the reference compiler on ./valid/scope/scopeBasic.wacc
-- Test: scopeBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# very simple scoping test

# Output:

# Program:

begin
  skip;
  begin
    skip
  end
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeBasic.s contents are:
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
===========================================================
-- Finished

