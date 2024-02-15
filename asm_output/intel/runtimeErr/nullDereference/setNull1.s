./valid/runtimeErr/nullDereference/setNull1.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/setNull1.wacc
-- Test: setNull1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by setting an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	fst p = 1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
setNull1.s contents are:
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
12		mov rax, 0
13		mov r12, rax
14		cmp r12, 0
15		je _errNull
16		mov rax, 1
17		mov qword ptr [r12], rax
18		# Stack pointer unchanged, no stack allocated variables
19		mov rax, 0
20		# pop {rbx, r12}
21		mov rbx, qword ptr [rsp]
22		mov r12, qword ptr [rsp + 8]
23		add rsp, 16
24		pop rbp
25		ret
26	
27	.section .rodata
28	# length of .L._prints_str0
29		.int 4
30	.L._prints_str0:
31		.asciz "%.*s"
32	.text
33	_prints:
34		push rbp
35		mov rbp, rsp
36		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
37		and rsp, -16
38		mov rdx, rdi
39		mov esi, dword ptr [rdi - 4]
40		lea rdi, [rip + .L._prints_str0]
41		# on x86, al represents the number of SIMD registers used as variadic arguments
42		mov al, 0
43		call printf@plt
44		mov rdi, 0
45		call fflush@plt
46		mov rsp, rbp
47		pop rbp
48		ret
49	
50	_exit:
51		push rbp
52		mov rbp, rsp
53		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
54		and rsp, -16
55		call exit@plt
56		mov rsp, rbp
57		pop rbp
58		ret
59	
60	.section .rodata
61	# length of .L._errNull_str0
62		.int 45
63	.L._errNull_str0:
64		.asciz "fatal error: null pair dereferenced or freed\n"
65	.text
66	_errNull:
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		and rsp, -16
69		lea rdi, [rip + .L._errNull_str0]
70		call _prints
71		mov dil, -1
72		call exit@plt
===========================================================
-- Finished

