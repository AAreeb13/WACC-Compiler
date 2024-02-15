./valid/pairs/createPair03.wacc
calling the reference compiler on ./valid/pairs/createPair03.wacc
-- Test: createPair03.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, char)

# Output:

# Program:

begin
  pair(int, char) p = newpair(10, 'a')
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createPair03.s contents are:
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
12		mov edi, 16
13		call _malloc
14		mov r11, rax
15		mov rax, 10
16		mov qword ptr [r11], rax
17		mov rax, 97
18		mov qword ptr [r11 + 8], rax
19		mov rax, r11
20		mov r12, rax
21		# Stack pointer unchanged, no stack allocated variables
22		mov rax, 0
23		# pop {rbx, r12}
24		mov rbx, qword ptr [rsp]
25		mov r12, qword ptr [rsp + 8]
26		add rsp, 16
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
53	_malloc:
54		push rbp
55		mov rbp, rsp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		and rsp, -16
58		call malloc@plt
59		cmp rax, 0
60		je _errOutOfMemory
61		mov rsp, rbp
62		pop rbp
63		ret
64	
65	_exit:
66		push rbp
67		mov rbp, rsp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		and rsp, -16
70		call exit@plt
71		mov rsp, rbp
72		pop rbp
73		ret
74	
75	.section .rodata
76	# length of .L._errOutOfMemory_str0
77		.int 27
78	.L._errOutOfMemory_str0:
79		.asciz "fatal error: out of memory\n"
80	.text
81	_errOutOfMemory:
82		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
83		and rsp, -16
84		lea rdi, [rip + .L._errOutOfMemory_str0]
85		call _prints
86		mov dil, -1
87		call exit@plt
===========================================================
-- Finished

