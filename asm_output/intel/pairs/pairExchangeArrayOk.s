./valid/pairs/pairExchangeArrayOk.wacc
calling the reference compiler on ./valid/pairs/pairExchangeArrayOk.wacc
-- Test: pairExchangeArrayOk.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Assignment is legal when assigning array (even of unknown type) in nested pair extraction
# Thanks to Kristina Zimina for catching this

# Output:

# Program:

begin
  pair(int, int) p = newpair(4, 5);
  pair(pair, int) q = newpair(p, 6);
  fst fst q = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairExchangeArrayOk.s contents are:
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
16		mov rax, 4
17		mov qword ptr [r11], rax
18		mov rax, 5
19		mov qword ptr [r11 + 8], rax
20		mov rax, r11
21		mov r12, rax
22		mov edi, 16
23		call _malloc
24		mov r11, rax
25		mov rax, r12
26		mov qword ptr [r11], rax
27		mov rax, 6
28		mov qword ptr [r11 + 8], rax
29		mov rax, r11
30		mov r13, rax
31		# 0 element array
32		mov edi, 4
33		call _malloc
34		mov r11, rax
35		# array pointers are shifted forwards by 4 bytes (to account for size)
36		mov r11, r11
37		add r11, 4
38		mov rax, 0
39		mov dword ptr [r11 - 4], eax
40		cmp r13, 0
41		je _errNull
42		mov rax, qword ptr [r13]
43		mov rax, rax
44		push rax
45		pop rbx
46		cmp rbx, 0
47		je _errNull
48		mov rax, r11
49		mov qword ptr [rbx], rax
50		# Stack pointer unchanged, no stack allocated variables
51		mov rax, 0
52		# pop {rbx, r12, r13}
53		mov rbx, qword ptr [rsp]
54		mov r12, qword ptr [rsp + 8]
55		mov r13, qword ptr [rsp + 16]
56		add rsp, 24
57		pop rbp
58		ret
59	
60	.section .rodata
61	# length of .L._prints_str0
62		.int 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.text
66	_prints:
67		push rbp
68		mov rbp, rsp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		and rsp, -16
71		mov rdx, rdi
72		mov esi, dword ptr [rdi - 4]
73		lea rdi, [rip + .L._prints_str0]
74		# on x86, al represents the number of SIMD registers used as variadic arguments
75		mov al, 0
76		call printf@plt
77		mov rdi, 0
78		call fflush@plt
79		mov rsp, rbp
80		pop rbp
81		ret
82	
83	_malloc:
84		push rbp
85		mov rbp, rsp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		call malloc@plt
89		cmp rax, 0
90		je _errOutOfMemory
91		mov rsp, rbp
92		pop rbp
93		ret
94	
95	_exit:
96		push rbp
97		mov rbp, rsp
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		and rsp, -16
100		call exit@plt
101		mov rsp, rbp
102		pop rbp
103		ret
104	
105	.section .rodata
106	# length of .L._errOutOfMemory_str0
107		.int 27
108	.L._errOutOfMemory_str0:
109		.asciz "fatal error: out of memory\n"
110	.text
111	_errOutOfMemory:
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		and rsp, -16
114		lea rdi, [rip + .L._errOutOfMemory_str0]
115		call _prints
116		mov dil, -1
117		call exit@plt
118	
119	.section .rodata
120	# length of .L._errNull_str0
121		.int 45
122	.L._errNull_str0:
123		.asciz "fatal error: null pair dereferenced or freed\n"
124	.text
125	_errNull:
126		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
127		and rsp, -16
128		lea rdi, [rip + .L._errNull_str0]
129		call _prints
130		mov dil, -1
131		call exit@plt
===========================================================
-- Finished

