./valid/pairs/nestedPairRightExtract.wacc
calling the reference compiler on ./valid/pairs/nestedPairRightExtract.wacc
-- Test: nestedPairRightExtract.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair extractions are legal as long as the left hand-side type is known

# Output:
# 2
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  int x = fst snd q ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairRightExtract.s contents are:
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
37		pop rax
38		cmp rax, 0
39		je _errNull
40		mov rax, qword ptr [rax]
41		mov rax, rax
42		mov r14, rax
43		# Stack pointer unchanged, no stack allocated arguments
44		mov rax, r14
45		mov rdi, rax
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _printi
48		call _println
49		# Stack pointer unchanged, no stack allocated variables
50		mov rax, 0
51		# pop {rbx, r12, r13, r14}
52		mov rbx, qword ptr [rsp]
53		mov r12, qword ptr [rsp + 8]
54		mov r13, qword ptr [rsp + 16]
55		mov r14, qword ptr [rsp + 24]
56		add rsp, 32
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
95	.section .rodata
96	# length of .L._printi_str0
97		.int 2
98	.L._printi_str0:
99		.asciz "%d"
100	.text
101	_printi:
102		push rbp
103		mov rbp, rsp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		and rsp, -16
106		mov esi, edi
107		lea rdi, [rip + .L._printi_str0]
108		# on x86, al represents the number of SIMD registers used as variadic arguments
109		mov al, 0
110		call printf@plt
111		mov rdi, 0
112		call fflush@plt
113		mov rsp, rbp
114		pop rbp
115		ret
116	
117	.section .rodata
118	# length of .L._println_str0
119		.int 0
120	.L._println_str0:
121		.asciz ""
122	.text
123	_println:
124		push rbp
125		mov rbp, rsp
126		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
127		and rsp, -16
128		lea rdi, [rip + .L._println_str0]
129		call puts@plt
130		mov rdi, 0
131		call fflush@plt
132		mov rsp, rbp
133		pop rbp
134		ret
135	
136	_exit:
137		push rbp
138		mov rbp, rsp
139		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
140		and rsp, -16
141		call exit@plt
142		mov rsp, rbp
143		pop rbp
144		ret
145	
146	.section .rodata
147	# length of .L._errOutOfMemory_str0
148		.int 27
149	.L._errOutOfMemory_str0:
150		.asciz "fatal error: out of memory\n"
151	.text
152	_errOutOfMemory:
153		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
154		and rsp, -16
155		lea rdi, [rip + .L._errOutOfMemory_str0]
156		call _prints
157		mov dil, -1
158		call exit@plt
159	
160	.section .rodata
161	# length of .L._errNull_str0
162		.int 45
163	.L._errNull_str0:
164		.asciz "fatal error: null pair dereferenced or freed\n"
165	.text
166	_errNull:
167		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
168		and rsp, -16
169		lea rdi, [rip + .L._errNull_str0]
170		call _prints
171		mov dil, -1
172		call exit@plt
===========================================================
-- Finished

