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
===========================================================
-- Finished

