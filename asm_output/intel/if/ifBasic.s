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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		mov rax, 1
9		cmp rax, 1
10		je .L0
11		jmp .L1
12	.L0:
13	.L1:
14		mov rax, 0
15		pop rbx
16		pop rbp
17		ret
===========================================================
-- Finished

