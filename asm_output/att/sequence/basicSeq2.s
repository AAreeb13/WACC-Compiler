./valid/sequence/basicSeq2.wacc
calling the reference compiler on ./valid/sequence/basicSeq2.wacc
-- Test: basicSeq2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic sequential composition

# Output:

# Program:

begin
  skip;
  skip;
  skip
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
basicSeq2.s contents are:
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

