./valid/scope/intsAndKeywords.wacc
calling the reference compiler on ./valid/scope/intsAndKeywords.wacc
-- Test: intsAndKeywords.wacc

-- Uploaded file: 
---------------------------------------------------------------
# checking handling of ints and keywords

# Output:

# Program:
begin
  begin
    int x = 125end
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intsAndKeywords.s contents are:
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
12		mov rax, 125
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated variables
15		mov rax, 0
16		# pop {rbx, r12}
17		mov rbx, qword ptr [rsp]
18		mov r12, qword ptr [rsp + 8]
19		add rsp, 16
20		pop rbp
21		ret
===========================================================
-- Finished

