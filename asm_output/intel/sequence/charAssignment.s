./valid/sequence/charAssignment.wacc
calling the reference compiler on ./valid/sequence/charAssignment.wacc
-- Test: charAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple character variable declaration and assignment

# Output:

# Program:

begin
  char c = 'a' ;
  c = 'Z'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charAssignment.s contents are:
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
12		mov rax, 97
13		mov r12, rax
14		mov rax, 90
15		mov r12, rax
16		# Stack pointer unchanged, no stack allocated variables
17		mov rax, 0
18		# pop {rbx, r12}
19		mov rbx, qword ptr [rsp]
20		mov r12, qword ptr [rsp + 8]
21		add rsp, 16
22		pop rbp
23		ret
===========================================================
-- Finished

