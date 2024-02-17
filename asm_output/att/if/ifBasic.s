./valid/if/ifBasic.wacc
calling the reference compiler on ./valid/if/ifBasic.wacc
-- Test: ifBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple if statement

# Output:

# Program:

begin
  if true
  then
    skip
  else
    skip
  fi
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifBasic.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		movq $1, %rax
8		cmpq $1, %rax
9		je .L0
10		jmp .L1
11	.L0:
12	.L1:
13		movq $0, %rax
14		popq %rbx
15		popq %rbp
16		ret
===========================================================
-- Finished

