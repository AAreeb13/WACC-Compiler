./valid/IO/print/printCharAsString.wacc
calling the reference compiler on ./valid/IO/print/printCharAsString.wacc
-- Test: printCharAsString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# character array treated as a string

# Output:
# foo
# bar
#

# Program:

begin
  char[] str = ['f','o','o'];
  println str;
  str = ['b','a','r'];
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharAsString.s contents are:
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
12		# 3 element array
13		mov edi, 7
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 3
20		mov dword ptr [r11 - 4], eax
21		mov rax, 102
22		mov byte ptr [r11], al
23		mov rax, 111
24		mov byte ptr [r11 + 1], al
25		mov rax, 111
26		mov byte ptr [r11 + 2], al
27		mov rax, r11
28		mov r12, rax
29		# Stack pointer unchanged, no stack allocated arguments
30		mov rax, r12
31		mov rdi, rax
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _prints
34		call _println
35		# 3 element array
36		mov edi, 7
37		call _malloc
38		mov r11, rax
39		# array pointers are shifted forwards by 4 bytes (to account for size)
40		mov r11, r11
41		add r11, 4
42		mov rax, 3
43		mov dword ptr [r11 - 4], eax
44		mov rax, 98
45		mov byte ptr [r11], al
46		mov rax, 97
47		mov byte ptr [r11 + 1], al
48		mov rax, 114
49		mov byte ptr [r11 + 2], al
50		mov rax, r11
51		mov r12, rax
52		# Stack pointer unchanged, no stack allocated arguments
53		mov rax, r12
54		mov rdi, rax
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _prints
57		call _println
58		# Stack pointer unchanged, no stack allocated variables
59		mov rax, 0
60		# pop {rbx, r12}
61		mov rbx, qword ptr [rsp]
62		mov r12, qword ptr [rsp + 8]
63		add rsp, 16
64		pop rbp
65		ret
66	
67	.section .rodata
68	# length of .L._prints_str0
69		.int 4
70	.L._prints_str0:
71		.asciz "%.*s"
72	.text
73	_prints:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		mov rdx, rdi
79		mov esi, dword ptr [rdi - 4]
80		lea rdi, [rip + .L._prints_str0]
81		# on x86, al represents the number of SIMD registers used as variadic arguments
82		mov al, 0
83		call printf@plt
84		mov rdi, 0
85		call fflush@plt
86		mov rsp, rbp
87		pop rbp
88		ret
89	
90	_malloc:
91		push rbp
92		mov rbp, rsp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		and rsp, -16
95		call malloc@plt
96		cmp rax, 0
97		je _errOutOfMemory
98		mov rsp, rbp
99		pop rbp
100		ret
101	
102	.section .rodata
103	# length of .L._println_str0
104		.int 0
105	.L._println_str0:
106		.asciz ""
107	.text
108	_println:
109		push rbp
110		mov rbp, rsp
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		and rsp, -16
113		lea rdi, [rip + .L._println_str0]
114		call puts@plt
115		mov rdi, 0
116		call fflush@plt
117		mov rsp, rbp
118		pop rbp
119		ret
120	
121	_exit:
122		push rbp
123		mov rbp, rsp
124		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
125		and rsp, -16
126		call exit@plt
127		mov rsp, rbp
128		pop rbp
129		ret
130	
131	.section .rodata
132	# length of .L._errOutOfMemory_str0
133		.int 27
134	.L._errOutOfMemory_str0:
135		.asciz "fatal error: out of memory\n"
136	.text
137	_errOutOfMemory:
138		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
139		and rsp, -16
140		lea rdi, [rip + .L._errOutOfMemory_str0]
141		call _prints
142		mov dil, -1
143		call exit@plt
===========================================================
-- Finished

