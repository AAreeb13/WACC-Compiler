./valid/runtimeErr/badChar/negativeChr.wacc
calling the reference compiler on ./valid/runtimeErr/badChar/negativeChr.wacc
-- Test: negativeChr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# try to convert negative int to character

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
    char c = chr -1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negativeChr.s contents are:
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
12		mov rax, -1
13		test rax, -128
14		cmovne rsi, rax
15		jne _errBadChar
16		push rax
17		pop rax
18		mov rax, rax
19		mov r12, rax
20		# Stack pointer unchanged, no stack allocated variables
21		mov rax, 0
22		# pop {rbx, r12}
23		mov rbx, qword ptr [rsp]
24		mov r12, qword ptr [rsp + 8]
25		add rsp, 16
26		pop rbp
27		ret
28	
29	.section .rodata
30	# length of .L._prints_str0
31		.int 4
32	.L._prints_str0:
33		.asciz "%.*s"
34	.text
35	_prints:
36		push rbp
37		mov rbp, rsp
38		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
39		and rsp, -16
40		mov rdx, rdi
41		mov esi, dword ptr [rdi - 4]
42		lea rdi, [rip + .L._prints_str0]
43		# on x86, al represents the number of SIMD registers used as variadic arguments
44		mov al, 0
45		call printf@plt
46		mov rdi, 0
47		call fflush@plt
48		mov rsp, rbp
49		pop rbp
50		ret
51	
52	_exit:
53		push rbp
54		mov rbp, rsp
55		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
56		and rsp, -16
57		call exit@plt
58		mov rsp, rbp
59		pop rbp
60		ret
61	
62	.section .rodata
63	# length of .L._errBadChar_str0
64		.int 50
65	.L._errBadChar_str0:
66		.asciz "fatal error: int %d is not ascii character 0-127 \n"
67	.text
68	_errBadChar:
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		and rsp, -16
71		lea rdi, [rip + .L._errBadChar_str0]
72		# on x86, al represents the number of SIMD registers used as variadic arguments
73		mov al, 0
74		call printf@plt
75		mov rdi, 0
76		call fflush@plt
77		mov dil, -1
78		call exit@plt
===========================================================
-- Finished

