./valid/array/modifyString.wacc
calling the reference compiler on ./valid/array/modifyString.wacc
-- Test: modifyString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and modify string as array of characters

# Output:
# hello world!
# Hello world!
# Hi!
#

# Program:

begin
  char[] str = ['h','e','l','l','o',' ','w','o','r','l','d','!'] ;
  println str ;
  str[0] = 'H' ;
  println str ;
  str = ['H','i','!'] ;
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
modifyString.s contents are:
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
12		# 12 element array
13		mov edi, 16
14		call _malloc
15		mov r11, rax
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		mov r11, r11
18		add r11, 4
19		mov rax, 12
20		mov dword ptr [r11 - 4], eax
21		mov rax, 104
22		mov byte ptr [r11], al
23		mov rax, 101
24		mov byte ptr [r11 + 1], al
25		mov rax, 108
26		mov byte ptr [r11 + 2], al
27		mov rax, 108
28		mov byte ptr [r11 + 3], al
29		mov rax, 111
30		mov byte ptr [r11 + 4], al
31		mov rax, 32
32		mov byte ptr [r11 + 5], al
33		mov rax, 119
34		mov byte ptr [r11 + 6], al
35		mov rax, 111
36		mov byte ptr [r11 + 7], al
37		mov rax, 114
38		mov byte ptr [r11 + 8], al
39		mov rax, 108
40		mov byte ptr [r11 + 9], al
41		mov rax, 100
42		mov byte ptr [r11 + 10], al
43		mov rax, 33
44		mov byte ptr [r11 + 11], al
45		mov rax, r11
46		mov r12, rax
47		# Stack pointer unchanged, no stack allocated arguments
48		mov rax, r12
49		mov rdi, rax
50		# statement primitives do not return results (but will clobber r0/rax)
51		call _prints
52		call _println
53		mov r10d, 0
54		mov rax, 72
55		mov r9, r12
56		call _arrStore1
57		# Stack pointer unchanged, no stack allocated arguments
58		mov rax, r12
59		mov rdi, rax
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _prints
62		call _println
63		# 3 element array
64		mov edi, 7
65		call _malloc
66		mov r11, rax
67		# array pointers are shifted forwards by 4 bytes (to account for size)
68		mov r11, r11
69		add r11, 4
70		mov rax, 3
71		mov dword ptr [r11 - 4], eax
72		mov rax, 72
73		mov byte ptr [r11], al
74		mov rax, 105
75		mov byte ptr [r11 + 1], al
76		mov rax, 33
77		mov byte ptr [r11 + 2], al
78		mov rax, r11
79		mov r12, rax
80		# Stack pointer unchanged, no stack allocated arguments
81		mov rax, r12
82		mov rdi, rax
83		# statement primitives do not return results (but will clobber r0/rax)
84		call _prints
85		call _println
86		# Stack pointer unchanged, no stack allocated variables
87		mov rax, 0
88		# pop {rbx, r12}
89		mov rbx, qword ptr [rsp]
90		mov r12, qword ptr [rsp + 8]
91		add rsp, 16
92		pop rbp
93		ret
94	
95	.section .rodata
96	# length of .L._prints_str0
97		.int 4
98	.L._prints_str0:
99		.asciz "%.*s"
100	.text
101	_prints:
102		push rbp
103		mov rbp, rsp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		and rsp, -16
106		mov rdx, rdi
107		mov esi, dword ptr [rdi - 4]
108		lea rdi, [rip + .L._prints_str0]
109		# on x86, al represents the number of SIMD registers used as variadic arguments
110		mov al, 0
111		call printf@plt
112		mov rdi, 0
113		call fflush@plt
114		mov rsp, rbp
115		pop rbp
116		ret
117	
118	_malloc:
119		push rbp
120		mov rbp, rsp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		and rsp, -16
123		call malloc@plt
124		cmp rax, 0
125		je _errOutOfMemory
126		mov rsp, rbp
127		pop rbp
128		ret
129	
130	.section .rodata
131	# length of .L._println_str0
132		.int 0
133	.L._println_str0:
134		.asciz ""
135	.text
136	_println:
137		push rbp
138		mov rbp, rsp
139		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
140		and rsp, -16
141		lea rdi, [rip + .L._println_str0]
142		call puts@plt
143		mov rdi, 0
144		call fflush@plt
145		mov rsp, rbp
146		pop rbp
147		ret
148	
149	_exit:
150		push rbp
151		mov rbp, rsp
152		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
153		and rsp, -16
154		call exit@plt
155		mov rsp, rbp
156		pop rbp
157		ret
158	
159	_arrStore1:
160		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
161		push rbx
162		cmp r10d, 0
163		cmovl esi, r10d
164		jl _errOutOfBounds
165		mov ebx, dword ptr [r9 - 4]
166		cmp r10d, ebx
167		cmovge esi, r10d
168		jge _errOutOfBounds
169		mov byte ptr [r9 + r10], al
170		pop rbx
171		ret
172	
173	.section .rodata
174	# length of .L._errOutOfMemory_str0
175		.int 27
176	.L._errOutOfMemory_str0:
177		.asciz "fatal error: out of memory\n"
178	.text
179	_errOutOfMemory:
180		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
181		and rsp, -16
182		lea rdi, [rip + .L._errOutOfMemory_str0]
183		call _prints
184		mov dil, -1
185		call exit@plt
186	
187	.section .rodata
188	# length of .L._errOutOfBounds_str0
189		.int 42
190	.L._errOutOfBounds_str0:
191		.asciz "fatal error: array index %d out of bounds\n"
192	.text
193	_errOutOfBounds:
194		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
195		and rsp, -16
196		lea rdi, [rip + .L._errOutOfBounds_str0]
197		# on x86, al represents the number of SIMD registers used as variadic arguments
198		mov al, 0
199		call printf@plt
200		mov rdi, 0
201		call fflush@plt
202		mov dil, -1
203		call exit@plt
===========================================================
-- Finished

