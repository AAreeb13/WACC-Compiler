./valid/array/charArrayInStringArray.wacc
calling the reference compiler on ./valid/array/charArrayInStringArray.wacc
-- Test: charArrayInStringArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a char[] can fit into a string[]
# Thanks to Iurii Zamiatin for finding this

# Output:

# Program:

begin
    char[] testArr = ['a', 'b', 'c'];
    string[] arr = [testArr, "box", "foo", "bar"]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charArrayInStringArray.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 3
5	.L.str0:
6		.asciz "box"
7	# length of .L.str1
8		.int 3
9	.L.str1:
10		.asciz "foo"
11	# length of .L.str2
12		.int 3
13	.L.str2:
14		.asciz "bar"
15	.text
16	main:
17		push rbp
18		# push {rbx, r12, r13}
19		sub rsp, 24
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov rbp, rsp
24		# Stack pointer unchanged, no stack allocated variables
25		# 3 element array
26		mov edi, 7
27		call _malloc
28		mov r11, rax
29		# array pointers are shifted forwards by 4 bytes (to account for size)
30		mov r11, r11
31		add r11, 4
32		mov rax, 3
33		mov dword ptr [r11 - 4], eax
34		mov rax, 97
35		mov byte ptr [r11], al
36		mov rax, 98
37		mov byte ptr [r11 + 1], al
38		mov rax, 99
39		mov byte ptr [r11 + 2], al
40		mov rax, r11
41		mov r12, rax
42		# 4 element array
43		mov edi, 36
44		call _malloc
45		mov r11, rax
46		# array pointers are shifted forwards by 4 bytes (to account for size)
47		mov r11, r11
48		add r11, 4
49		mov rax, 4
50		mov dword ptr [r11 - 4], eax
51		mov rax, r12
52		mov qword ptr [r11], rax
53		lea rax, [rip + .L.str0]
54		push rax
55		pop rax
56		mov rax, rax
57		mov qword ptr [r11 + 8], rax
58		lea rax, [rip + .L.str1]
59		push rax
60		pop rax
61		mov rax, rax
62		mov qword ptr [r11 + 16], rax
63		lea rax, [rip + .L.str2]
64		push rax
65		pop rax
66		mov rax, rax
67		mov qword ptr [r11 + 24], rax
68		mov rax, r11
69		mov r13, rax
70		# Stack pointer unchanged, no stack allocated variables
71		mov rax, 0
72		# pop {rbx, r12, r13}
73		mov rbx, qword ptr [rsp]
74		mov r12, qword ptr [rsp + 8]
75		mov r13, qword ptr [rsp + 16]
76		add rsp, 24
77		pop rbp
78		ret
79	
80	.section .rodata
81	# length of .L._prints_str0
82		.int 4
83	.L._prints_str0:
84		.asciz "%.*s"
85	.text
86	_prints:
87		push rbp
88		mov rbp, rsp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		and rsp, -16
91		mov rdx, rdi
92		mov esi, dword ptr [rdi - 4]
93		lea rdi, [rip + .L._prints_str0]
94		# on x86, al represents the number of SIMD registers used as variadic arguments
95		mov al, 0
96		call printf@plt
97		mov rdi, 0
98		call fflush@plt
99		mov rsp, rbp
100		pop rbp
101		ret
102	
103	_malloc:
104		push rbp
105		mov rbp, rsp
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		and rsp, -16
108		call malloc@plt
109		cmp rax, 0
110		je _errOutOfMemory
111		mov rsp, rbp
112		pop rbp
113		ret
114	
115	_exit:
116		push rbp
117		mov rbp, rsp
118		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
119		and rsp, -16
120		call exit@plt
121		mov rsp, rbp
122		pop rbp
123		ret
124	
125	.section .rodata
126	# length of .L._errOutOfMemory_str0
127		.int 27
128	.L._errOutOfMemory_str0:
129		.asciz "fatal error: out of memory\n"
130	.text
131	_errOutOfMemory:
132		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
133		and rsp, -16
134		lea rdi, [rip + .L._errOutOfMemory_str0]
135		call _prints
136		mov dil, -1
137		call exit@plt
===========================================================
-- Finished

