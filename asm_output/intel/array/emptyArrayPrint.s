./valid/array/emptyArrayPrint.wacc
calling the reference compiler on ./valid/array/emptyArrayPrint.wacc
-- Test: emptyArrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and print after

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayPrint.s contents are:
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
12		# 0 element array
13		mov edi, 4
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 0
20		mov dword ptr [r11 - 4], eax
21		mov rax, r11
22		mov r12, rax
23		# Stack pointer unchanged, no stack allocated arguments
24		mov rax, 1
25		mov rdi, rax
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printb
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		mov rax, 0
31		# pop {rbx, r12}
32		mov rbx, qword ptr [rsp]
33		mov r12, qword ptr [rsp + 8]
34		add rsp, 16
35		pop rbp
36		ret
37	
38	.section .rodata
39	# length of .L._prints_str0
40		.int 4
41	.L._prints_str0:
42		.asciz "%.*s"
43	.text
44	_prints:
45		push rbp
46		mov rbp, rsp
47		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
48		and rsp, -16
49		mov rdx, rdi
50		mov esi, dword ptr [rdi - 4]
51		lea rdi, [rip + .L._prints_str0]
52		# on x86, al represents the number of SIMD registers used as variadic arguments
53		mov al, 0
54		call printf@plt
55		mov rdi, 0
56		call fflush@plt
57		mov rsp, rbp
58		pop rbp
59		ret
60	
61	_malloc:
62		push rbp
63		mov rbp, rsp
64		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
65		and rsp, -16
66		call malloc@plt
67		cmp rax, 0
68		je _errOutOfMemory
69		mov rsp, rbp
70		pop rbp
71		ret
72	
73	.section .rodata
74	# length of .L._printb_str0
75		.int 5
76	.L._printb_str0:
77		.asciz "false"
78	# length of .L._printb_str1
79		.int 4
80	.L._printb_str1:
81		.asciz "true"
82	# length of .L._printb_str2
83		.int 4
84	.L._printb_str2:
85		.asciz "%.*s"
86	.text
87	_printb:
88		push rbp
89		mov rbp, rsp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		and rsp, -16
92		cmp dil, 0
93		jne .L_printb0
94		lea rdx, [rip + .L._printb_str0]
95		jmp .L_printb1
96	.L_printb0:
97		lea rdx, [rip + .L._printb_str1]
98	.L_printb1:
99		mov esi, dword ptr [rdx - 4]
100		lea rdi, [rip + .L._printb_str2]
101		# on x86, al represents the number of SIMD registers used as variadic arguments
102		mov al, 0
103		call printf@plt
104		mov rdi, 0
105		call fflush@plt
106		mov rsp, rbp
107		pop rbp
108		ret
109	
110	.section .rodata
111	# length of .L._println_str0
112		.int 0
113	.L._println_str0:
114		.asciz ""
115	.text
116	_println:
117		push rbp
118		mov rbp, rsp
119		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
120		and rsp, -16
121		lea rdi, [rip + .L._println_str0]
122		call puts@plt
123		mov rdi, 0
124		call fflush@plt
125		mov rsp, rbp
126		pop rbp
127		ret
128	
129	_exit:
130		push rbp
131		mov rbp, rsp
132		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
133		and rsp, -16
134		call exit@plt
135		mov rsp, rbp
136		pop rbp
137		ret
138	
139	.section .rodata
140	# length of .L._errOutOfMemory_str0
141		.int 27
142	.L._errOutOfMemory_str0:
143		.asciz "fatal error: out of memory\n"
144	.text
145	_errOutOfMemory:
146		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
147		and rsp, -16
148		lea rdi, [rip + .L._errOutOfMemory_str0]
149		call _prints
150		mov dil, -1
151		call exit@plt
===========================================================
-- Finished

