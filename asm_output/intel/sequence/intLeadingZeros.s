./valid/sequence/intLeadingZeros.wacc
calling the reference compiler on ./valid/sequence/intLeadingZeros.wacc
-- Test: intLeadingZeros.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer variable declaration with leading zeroes

# Output:
# 42
# 0
#

# Program:

begin
  int x = 0000000000000000000000000000000000000042 ;
  int y = 0000000000000000000000000000000000000000 ;
  println x ;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intLeadingZeros.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13}
7		sub rsp, 24
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated variables
13		mov rax, 42
14		mov r12, rax
15		mov rax, 0
16		mov r13, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		mov rax, r12
19		mov rdi, rax
20		# statement primitives do not return results (but will clobber r0/rax)
21		call _printi
22		call _println
23		# Stack pointer unchanged, no stack allocated arguments
24		mov rax, r13
25		mov rdi, rax
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		mov rax, 0
31		# pop {rbx, r12, r13}
32		mov rbx, qword ptr [rsp]
33		mov r12, qword ptr [rsp + 8]
34		mov r13, qword ptr [rsp + 16]
35		add rsp, 24
36		pop rbp
37		ret
38	
39	.section .rodata
40	# length of .L._printi_str0
41		.int 2
42	.L._printi_str0:
43		.asciz "%d"
44	.text
45	_printi:
46		push rbp
47		mov rbp, rsp
48		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
49		and rsp, -16
50		mov esi, edi
51		lea rdi, [rip + .L._printi_str0]
52		# on x86, al represents the number of SIMD registers used as variadic arguments
53		mov al, 0
54		call printf@plt
55		mov rdi, 0
56		call fflush@plt
57		mov rsp, rbp
58		pop rbp
59		ret
60	
61	.section .rodata
62	# length of .L._println_str0
63		.int 0
64	.L._println_str0:
65		.asciz ""
66	.text
67	_println:
68		push rbp
69		mov rbp, rsp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		lea rdi, [rip + .L._println_str0]
73		call puts@plt
74		mov rdi, 0
75		call fflush@plt
76		mov rsp, rbp
77		pop rbp
78		ret
===========================================================
-- Finished

