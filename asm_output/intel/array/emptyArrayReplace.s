./valid/array/emptyArrayReplace.wacc
calling the reference compiler on ./valid/array/emptyArrayReplace.wacc
-- Test: emptyArrayReplace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# it should be possible to reassign to an array

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  x = [0] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayReplace.s contents are:
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
23		# 1 element array
24		mov edi, 8
25		call _malloc
26		mov r11, rax
27		# array pointers are shifted forwards by 4 bytes (to account for size)
28		mov r11, r11
29		add r11, 4
30		mov rax, 1
31		mov dword ptr [r11 - 4], eax
32		mov rax, 0
33		mov dword ptr [r11], eax
34		mov rax, r11
35		mov r12, rax
36		# Stack pointer unchanged, no stack allocated arguments
37		mov rax, 1
38		mov rdi, rax
39		# statement primitives do not return results (but will clobber r0/rax)
40		call _printb
41		call _println
42		# Stack pointer unchanged, no stack allocated variables
43		mov rax, 0
44		# pop {rbx, r12}
45		mov rbx, qword ptr [rsp]
46		mov r12, qword ptr [rsp + 8]
47		add rsp, 16
48		pop rbp
49		ret
50	
51	.section .rodata
52	# length of .L._prints_str0
53		.int 4
54	.L._prints_str0:
55		.asciz "%.*s"
56	.text
57	_prints:
58		push rbp
59		mov rbp, rsp
60		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
61		and rsp, -16
62		mov rdx, rdi
63		mov esi, dword ptr [rdi - 4]
64		lea rdi, [rip + .L._prints_str0]
65		# on x86, al represents the number of SIMD registers used as variadic arguments
66		mov al, 0
67		call printf@plt
68		mov rdi, 0
69		call fflush@plt
70		mov rsp, rbp
71		pop rbp
72		ret
73	
74	_malloc:
75		push rbp
76		mov rbp, rsp
77		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
78		and rsp, -16
79		call malloc@plt
80		cmp rax, 0
81		je _errOutOfMemory
82		mov rsp, rbp
83		pop rbp
84		ret
85	
86	.section .rodata
87	# length of .L._printb_str0
88		.int 5
89	.L._printb_str0:
90		.asciz "false"
91	# length of .L._printb_str1
92		.int 4
93	.L._printb_str1:
94		.asciz "true"
95	# length of .L._printb_str2
96		.int 4
97	.L._printb_str2:
98		.asciz "%.*s"
99	.text
100	_printb:
101		push rbp
102		mov rbp, rsp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		and rsp, -16
105		cmp dil, 0
106		jne .L_printb0
107		lea rdx, [rip + .L._printb_str0]
108		jmp .L_printb1
109	.L_printb0:
110		lea rdx, [rip + .L._printb_str1]
111	.L_printb1:
112		mov esi, dword ptr [rdx - 4]
113		lea rdi, [rip + .L._printb_str2]
114		# on x86, al represents the number of SIMD registers used as variadic arguments
115		mov al, 0
116		call printf@plt
117		mov rdi, 0
118		call fflush@plt
119		mov rsp, rbp
120		pop rbp
121		ret
122	
123	.section .rodata
124	# length of .L._println_str0
125		.int 0
126	.L._println_str0:
127		.asciz ""
128	.text
129	_println:
130		push rbp
131		mov rbp, rsp
132		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
133		and rsp, -16
134		lea rdi, [rip + .L._println_str0]
135		call puts@plt
136		mov rdi, 0
137		call fflush@plt
138		mov rsp, rbp
139		pop rbp
140		ret
141	
142	_exit:
143		push rbp
144		mov rbp, rsp
145		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
146		and rsp, -16
147		call exit@plt
148		mov rsp, rbp
149		pop rbp
150		ret
151	
152	.section .rodata
153	# length of .L._errOutOfMemory_str0
154		.int 27
155	.L._errOutOfMemory_str0:
156		.asciz "fatal error: out of memory\n"
157	.text
158	_errOutOfMemory:
159		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
160		and rsp, -16
161		lea rdi, [rip + .L._errOutOfMemory_str0]
162		call _prints
163		mov dil, -1
164		call exit@plt
===========================================================
-- Finished

