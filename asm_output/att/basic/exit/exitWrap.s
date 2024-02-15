./valid/basic/exit/exitWrap.wacc
calling the reference compiler on ./valid/basic/exit/exitWrap.wacc
-- Test: exitWrap.wacc

-- Uploaded file: 
---------------------------------------------------------------
# exit with wrapped int

# Output:

# Exit:
# 0

# Program:

begin
  exit 256
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exitWrap.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		# Stack pointer unchanged, no stack allocated arguments
8		movq $256, %rax
9		movq %rax, %rdi
10		# statement primitives do not return results (but will clobber r0/rax)
11		call _exit
12		movq $0, %rax
13		popq %rbx
14		popq %rbp
15		ret
16	
17	_exit:
18		pushq %rbp
19		movq %rsp, %rbp
20		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
21		andq $-16, %rsp
22		call exit@plt
23		movq %rbp, %rsp
24		popq %rbp
25		ret
===========================================================
-- Finished

