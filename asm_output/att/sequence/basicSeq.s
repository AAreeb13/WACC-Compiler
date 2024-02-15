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

