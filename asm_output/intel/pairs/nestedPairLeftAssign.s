./valid/pairs/nestedPairLeftAssign.wacc
calling the reference compiler on ./valid/pairs/nestedPairLeftAssign.wacc
-- Test: nestedPairLeftAssign.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair assignments are legal as long as the right hand-side type is known

# Output:
# 7
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  fst snd q = 7 ;
  int x = fst p ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairLeftAssign.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14}
7		sub rsp, 32
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov rbp, rsp
13		# Stack pointer unchanged, no stack allocated variables
14		mov edi, 16
15		call _malloc
16		mov r11, rax
17		mov rax, 2
18		mov qword ptr [r11], rax
19		mov rax, 3
20		mov qword ptr [r11 + 8], rax
21		mov rax, r11
22		mov r12, rax
23		mov edi, 16
24		call _malloc
25		mov r11, rax
26		mov rax, 1
27		mov qword ptr [r11], rax
28		mov rax, r12
29		mov qword ptr [r11 + 8], rax
30		mov rax, r11
31		mov r13, rax
32		cmp r13, 0
33		je _errNull
34		mov rax, qword ptr [r13 + 8]
35		mov rax, rax
36		push rax
37		pop rbx
38		cmp rbx, 0
39		je _errNull
40		mov rax, 7
41		mov qword ptr [rbx], rax
42		cmp r12, 0
43		je _errNull
44		mov rax, qword ptr [r12]
45		mov rax, rax
46		mov r14, rax
47		# Stack pointer unchanged, no stack allocated arguments
48		mov rax, r14
49		mov rdi, rax
50		# statement primitives do not return results (but will clobber r0/rax)
51		call _printi
52		call _println
53		# Stack pointer unchanged, no stack allocated variables
54		mov rax, 0
55		# pop {rbx, r12, r13, r14}
56		mov rbx, qword ptr [rsp]
57		mov r12, qword ptr [rsp + 8]
58		mov r13, qword ptr [rsp + 16]
59		mov r14, qword ptr [rsp + 24]
60		add rsp, 32
61		pop rbp
62		ret
63	
64	.section .rodata
65	# length of .L._prints_str0
66		.int 4
67	.L._prints_str0:
68		.asciz "%.*s"
69	.text
70	_prints:
71		push rbp
72		mov rbp, rsp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		and rsp, -16
75		mov rdx, rdi
76		mov esi, dword ptr [rdi - 4]
77		lea rdi, [rip + .L._prints_str0]
78		# on x86, al represents the number of SIMD registers used as variadic arguments
79		mov al, 0
80		call printf@plt
81		mov rdi, 0
82		call fflush@plt
83		mov rsp, rbp
84		pop rbp
85		ret
86	
87	_malloc:
88		push rbp
89		mov rbp, rsp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		and rsp, -16
92		call malloc@plt
93		cmp rax, 0
94		je _errOutOfMemory
95		mov rsp, rbp
96		pop rbp
97		ret
98	
99	.section .rodata
100	# length of .L._printi_str0
101		.int 2
102	.L._printi_str0:
103		.asciz "%d"
104	.text
105	_printi:
106		push rbp
107		mov rbp, rsp
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		and rsp, -16
110		mov esi, edi
111		lea rdi, [rip + .L._printi_str0]
112		# on x86, al represents the number of SIMD registers used as variadic arguments
113		mov al, 0
114		call printf@plt
115		mov rdi, 0
116		call fflush@plt
117		mov rsp, rbp
118		pop rbp
119		ret
120	
121	.section .rodata
122	# length of .L._println_str0
123		.int 0
124	.L._println_str0:
125		.asciz ""
126	.text
127	_println:
128		push rbp
129		mov rbp, rsp
130		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
131		and rsp, -16
132		lea rdi, [rip + .L._println_str0]
133		call puts@plt
134		mov rdi, 0
135		call fflush@plt
136		mov rsp, rbp
137		pop rbp
138		ret
139	
140	_exit:
141		push rbp
142		mov rbp, rsp
143		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
144		and rsp, -16
145		call exit@plt
146		mov rsp, rbp
147		pop rbp
148		ret
149	
150	.section .rodata
151	# length of .L._errOutOfMemory_str0
152		.int 27
153	.L._errOutOfMemory_str0:
154		.asciz "fatal error: out of memory\n"
155	.text
156	_errOutOfMemory:
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		and rsp, -16
159		lea rdi, [rip + .L._errOutOfMemory_str0]
160		call _prints
161		mov dil, -1
162		call exit@plt
163	
164	.section .rodata
165	# length of .L._errNull_str0
166		.int 45
167	.L._errNull_str0:
168		.asciz "fatal error: null pair dereferenced or freed\n"
169	.text
170	_errNull:
171		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
172		and rsp, -16
173		lea rdi, [rip + .L._errNull_str0]
174		call _prints
175		mov dil, -1
176		call exit@plt
===========================================================
-- Finished

