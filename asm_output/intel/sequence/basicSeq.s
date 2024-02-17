./valid/sequence/basicSeq.wacc
calling the reference compiler on ./valid/sequence/basicSeq.wacc
-- Test: basicSeq.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic sequential composition

# Output:

# Program:

begin
  skip;
  skip
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
basicSeq.s contents are:
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

