./valid/sequence/intAssignment.wacc
calling the reference compiler on ./valid/sequence/intAssignment.wacc
-- Test: intAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer variable declaration and assignment

# Output:

# Exit:
# 20

# Program:

begin
  int x = 10 ;
  x = 20 ;
  exit x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intAssignment.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12}
7		sub rsp, 16
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov rbp, rsp
11		# Stack pointer unchanged, no stack allocated variables
12		mov rax, 10
13		mov r12, rax
14		mov rax, 20
15		mov r12, rax
16		# Stack pointer unchanged, no stack allocated arguments
17		mov rax, r12
18		mov rdi, rax
19		# statement primitives do not return results (but will clobber r0/rax)
20		call _exit
21		# Stack pointer unchanged, no stack allocated variables
22		mov rax, 0
23		# pop {rbx, r12}
24		mov rbx, qword ptr [rsp]
25		mov r12, qword ptr [rsp + 8]
26		add rsp, 16
27		pop rbp
28		ret
29	
30	_exit:
31		push rbp
32		mov rbp, rsp
33		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
34		and rsp, -16
35		call exit@plt
36		mov rsp, rbp
37		pop rbp
38		ret
===========================================================
-- Finished

