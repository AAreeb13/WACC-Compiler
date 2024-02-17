./valid/runtimeErr/nullDereference/useNull2.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/useNull2.wacc
-- Test: useNull2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by using it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	int x = snd p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
useNull2.s contents are:
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
13		mov rax, 0
14		mov r12, rax
15		cmp r12, 0
16		je _errNull
17		mov rax, qword ptr [r12 + 8]
18		mov rax, rax
19		mov r13, rax
20		# Stack pointer unchanged, no stack allocated variables
21		mov rax, 0
22		# pop {rbx, r12, r13}
23		mov rbx, qword ptr [rsp]
24		mov r12, qword ptr [rsp + 8]
25		mov r13, qword ptr [rsp + 16]
26		add rsp, 24
27		pop rbp
28		ret
29	
30	.section .rodata
31	# length of .L._prints_str0
32		.int 4
33	.L._prints_str0:
34		.asciz "%.*s"
35	.text
36	_prints:
37		push rbp
38		mov rbp, rsp
39		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
40		and rsp, -16
41		mov rdx, rdi
42		mov esi, dword ptr [rdi - 4]
43		lea rdi, [rip + .L._prints_str0]
44		# on x86, al represents the number of SIMD registers used as variadic arguments
45		mov al, 0
46		call printf@plt
47		mov rdi, 0
48		call fflush@plt
49		mov rsp, rbp
50		pop rbp
51		ret
52	
53	_exit:
54		push rbp
55		mov rbp, rsp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		and rsp, -16
58		call exit@plt
59		mov rsp, rbp
60		pop rbp
61		ret
62	
63	.section .rodata
64	# length of .L._errNull_str0
65		.int 45
66	.L._errNull_str0:
67		.asciz "fatal error: null pair dereferenced or freed\n"
68	.text
69	_errNull:
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		lea rdi, [rip + .L._errNull_str0]
73		call _prints
74		mov dil, -1
75		call exit@plt
===========================================================
-- Finished

