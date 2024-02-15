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
12		mov rax, 19
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _exit
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 0
21		# pop {rbx, r12}
22		mov rbx, qword ptr [rsp]
23		mov r12, qword ptr [rsp + 8]
24		add rsp, 16
25		pop rbp
26		ret
27	
28	_exit:
29		push rbp
30		mov rbp, rsp
31		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
32		and rsp, -16
33		call exit@plt
34		mov rsp, rbp
35		pop rbp
36		ret
===========================================================
-- Finished

