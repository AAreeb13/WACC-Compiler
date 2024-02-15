./valid/variables/_VarNames.wacc
calling the reference compiler on ./valid/variables/_VarNames.wacc
-- Test: _VarNames.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable can have _ in their names

# Output:

# Exit:
# 19

# Program:

begin
  int an_underscore = 19 ;
  exit an_underscore
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
_VarNames.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12}
6		subq $16, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %rsp, %rbp
10		# Stack pointer unchanged, no stack allocated variables
11		movq $19, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _exit
18		# Stack pointer unchanged, no stack allocated variables
19		movq $0, %rax
20		# popq {%rbx, %r12}
21		movq (%rsp), %rbx
22		movq 8(%rsp), %r12
23		addq $16, %rsp
24		popq %rbp
25		ret
26	
27	_exit:
28		pushq %rbp
29		movq %rsp, %rbp
30		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
31		andq $-16, %rsp
32		call exit@plt
33		movq %rbp, %rsp
34		popq %rbp
35		ret
===========================================================
-- Finished

