./valid/basic/exit/exit-1.wacc
calling the reference compiler on ./valid/basic/exit/exit-1.wacc
-- Test: exit-1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# common error exit statement

# Output:

# Exit:
# 255

# Program:

begin
  exit -1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exit-1.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		# Stack pointer unchanged, no stack allocated arguments
9		mov rax, -1
10		mov rdi, rax
11		# statement primitives do not return results (but will clobber r0/rax)
12		call _exit
13		mov rax, 0
14		pop rbx
15		pop rbp
16		ret
17	
18	_exit:
19		push rbp
20		mov rbp, rsp
21		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
22		and rsp, -16
23		call exit@plt
24		mov rsp, rbp
25		pop rbp
26		ret
===========================================================
-- Finished

