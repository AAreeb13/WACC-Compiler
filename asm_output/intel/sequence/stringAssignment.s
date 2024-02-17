./valid/sequence/stringAssignment.wacc
calling the reference compiler on ./valid/sequence/stringAssignment.wacc
-- Test: stringAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple string variable declaration and assignment

# Output:

# Program:

begin
  string s = "foo" ;
  s = "bar"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringAssignment.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 3
5	.L.str0:
6		.asciz "foo"
7	# length of .L.str1
8		.int 3
9	.L.str1:
10		.asciz "bar"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		lea rax, [rip + .L.str0]
21		push rax
22		pop rax
23		mov rax, rax
24		mov r12, rax
25		lea rax, [rip + .L.str1]
26		push rax
27		pop rax
28		mov rax, rax
29		mov r12, rax
30		# Stack pointer unchanged, no stack allocated variables
31		mov rax, 0
32		# pop {rbx, r12}
33		mov rbx, qword ptr [rsp]
34		mov r12, qword ptr [rsp + 8]
35		add rsp, 16
36		pop rbp
37		ret
===========================================================
-- Finished

