./valid/variables/stringCarriageReturn.wacc
calling the reference compiler on ./valid/variables/stringCarriageReturn.wacc
-- Test: stringCarriageReturn.wacc

-- Uploaded file: 
---------------------------------------------------------------
# carriage returns should be parsable, but their behaviour is non-portable, so the IO is not currently tested

# Output:

# Program:

begin
  string s = "Hello \r World!\n"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringCarriageReturn.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 15
5	.L.str0:
6		.asciz "Hello \r World!\n"
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		lea rax, [rip + .L.str0]
17		push rax
18		pop rax
19		mov rax, rax
20		mov r12, rax
21		# Stack pointer unchanged, no stack allocated variables
22		mov rax, 0
23		# pop {rbx, r12}
24		mov rbx, qword ptr [rsp]
25		mov r12, qword ptr [rsp + 8]
26		add rsp, 16
27		pop rbp
28		ret
===========================================================
-- Finished

