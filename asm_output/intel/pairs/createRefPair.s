./valid/pairs/createRefPair.wacc
calling the reference compiler on ./valid/pairs/createRefPair.wacc
-- Test: createRefPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair(int, char) with a second reference to it

# Output:

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  pair(int, char) q = p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createRefPair.s contents are:
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
13		mov edi, 16
14		call _malloc
15		mov r11, rax
16		mov rax, 10
17		mov qword ptr [r11], rax
18		mov rax, 97
19		mov qword ptr [r11 + 8], rax
20		mov rax, r11
21		mov r12, rax
22		mov rax, r12
23		mov r13, rax
24		# Stack pointer unchanged, no stack allocated variables
25		mov rax, 0
26		# pop {rbx, r12, r13}
27		mov rbx, qword ptr [rsp]
28		mov r12, qword ptr [rsp + 8]
29		mov r13, qword ptr [rsp + 16]
30		add rsp, 24
31		pop rbp
32		ret
33	
34	.section .rodata
35	# length of .L._prints_str0
36		.int 4
37	.L._prints_str0:
38		.asciz "%.*s"
39	.text
40	_prints:
41		push rbp
42		mov rbp, rsp
43		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
44		and rsp, -16
45		mov rdx, rdi
46		mov esi, dword ptr [rdi - 4]
47		lea rdi, [rip + .L._prints_str0]
48		# on x86, al represents the number of SIMD registers used as variadic arguments
49		mov al, 0
50		call printf@plt
51		mov rdi, 0
52		call fflush@plt
53		mov rsp, rbp
54		pop rbp
55		ret
56	
57	_malloc:
58		push rbp
59		mov rbp, rsp
60		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
61		and rsp, -16
62		call malloc@plt
63		cmp rax, 0
64		je _errOutOfMemory
65		mov rsp, rbp
66		pop rbp
67		ret
68	
69	_exit:
70		push rbp
71		mov rbp, rsp
72		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
73		and rsp, -16
74		call exit@plt
75		mov rsp, rbp
76		pop rbp
77		ret
78	
79	.section .rodata
80	# length of .L._errOutOfMemory_str0
81		.int 27
82	.L._errOutOfMemory_str0:
83		.asciz "fatal error: out of memory\n"
84	.text
85	_errOutOfMemory:
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		lea rdi, [rip + .L._errOutOfMemory_str0]
89		call _prints
90		mov dil, -1
91		call exit@plt
===========================================================
-- Finished

