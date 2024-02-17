./valid/pairs/pairarray.wacc
calling the reference compiler on ./valid/pairs/pairarray.wacc
-- Test: pairarray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that pairs can be unpacked directly from arrays

# Output:
# 3
#

# Program:

begin
  pair(int, int) p = newpair(5, 6);
  pair(int, int)[] a = [p, p];
  fst a[0] = 3 ;
  int x = fst a[1] ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairarray.s contents are:
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
17		mov rax, 5
18		mov qword ptr [r11], rax
19		mov rax, 6
20		mov qword ptr [r11 + 8], rax
21		mov rax, r11
22		mov r12, rax
23		# 2 element array
24		mov edi, 20
25		call _malloc
26		mov r11, rax
27		# array pointers are shifted forwards by 4 bytes (to account for size)
28		mov r11, r11
29		add r11, 4
30		mov rax, 2
31		mov dword ptr [r11 - 4], eax
32		mov rax, r12
33		mov qword ptr [r11], rax
34		mov rax, r12
35		mov qword ptr [r11 + 8], rax
36		mov rax, r11
37		mov r13, rax
38		mov r10d, 0
39		mov r9, r13
40		call _arrLoad8
41		mov rax, r9
42		mov rax, rax
43		push rax
44		pop rbx
45		cmp rbx, 0
46		je _errNull
47		mov rax, 3
48		mov qword ptr [rbx], rax
49		mov r10d, 1
50		mov r9, r13
51		call _arrLoad8
52		mov rax, r9
53		mov rax, rax
54		push rax
55		pop rax
56		cmp rax, 0
57		je _errNull
58		mov rax, qword ptr [rax]
59		mov rax, rax
60		mov r14, rax
61		# Stack pointer unchanged, no stack allocated arguments
62		mov rax, r14
63		mov rdi, rax
64		# statement primitives do not return results (but will clobber r0/rax)
65		call _printi
66		call _println
67		# Stack pointer unchanged, no stack allocated variables
68		mov rax, 0
69		# pop {rbx, r12, r13, r14}
70		mov rbx, qword ptr [rsp]
71		mov r12, qword ptr [rsp + 8]
72		mov r13, qword ptr [rsp + 16]
73		mov r14, qword ptr [rsp + 24]
74		add rsp, 32
75		pop rbp
76		ret
77	
78	.section .rodata
79	# length of .L._prints_str0
80		.int 4
81	.L._prints_str0:
82		.asciz "%.*s"
83	.text
84	_prints:
85		push rbp
86		mov rbp, rsp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		and rsp, -16
89		mov rdx, rdi
90		mov esi, dword ptr [rdi - 4]
91		lea rdi, [rip + .L._prints_str0]
92		# on x86, al represents the number of SIMD registers used as variadic arguments
93		mov al, 0
94		call printf@plt
95		mov rdi, 0
96		call fflush@plt
97		mov rsp, rbp
98		pop rbp
99		ret
100	
101	_malloc:
102		push rbp
103		mov rbp, rsp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		and rsp, -16
106		call malloc@plt
107		cmp rax, 0
108		je _errOutOfMemory
109		mov rsp, rbp
110		pop rbp
111		ret
112	
113	.section .rodata
114	# length of .L._printi_str0
115		.int 2
116	.L._printi_str0:
117		.asciz "%d"
118	.text
119	_printi:
120		push rbp
121		mov rbp, rsp
122		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
123		and rsp, -16
124		mov esi, edi
125		lea rdi, [rip + .L._printi_str0]
126		# on x86, al represents the number of SIMD registers used as variadic arguments
127		mov al, 0
128		call printf@plt
129		mov rdi, 0
130		call fflush@plt
131		mov rsp, rbp
132		pop rbp
133		ret
134	
135	.section .rodata
136	# length of .L._println_str0
137		.int 0
138	.L._println_str0:
139		.asciz ""
140	.text
141	_println:
142		push rbp
143		mov rbp, rsp
144		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
145		and rsp, -16
146		lea rdi, [rip + .L._println_str0]
147		call puts@plt
148		mov rdi, 0
149		call fflush@plt
150		mov rsp, rbp
151		pop rbp
152		ret
153	
154	_exit:
155		push rbp
156		mov rbp, rsp
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		and rsp, -16
159		call exit@plt
160		mov rsp, rbp
161		pop rbp
162		ret
163	
164	_arrLoad8:
165		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
166		push rbx
167		cmp r10d, 0
168		cmovl esi, r10d
169		jl _errOutOfBounds
170		mov ebx, dword ptr [r9 - 4]
171		cmp r10d, ebx
172		cmovge esi, r10d
173		jge _errOutOfBounds
174		mov r9, qword ptr [r9 + 8*r10]
175		pop rbx
176		ret
177	
178	.section .rodata
179	# length of .L._errOutOfMemory_str0
180		.int 27
181	.L._errOutOfMemory_str0:
182		.asciz "fatal error: out of memory\n"
183	.text
184	_errOutOfMemory:
185		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
186		and rsp, -16
187		lea rdi, [rip + .L._errOutOfMemory_str0]
188		call _prints
189		mov dil, -1
190		call exit@plt
191	
192	.section .rodata
193	# length of .L._errNull_str0
194		.int 45
195	.L._errNull_str0:
196		.asciz "fatal error: null pair dereferenced or freed\n"
197	.text
198	_errNull:
199		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
200		and rsp, -16
201		lea rdi, [rip + .L._errNull_str0]
202		call _prints
203		mov dil, -1
204		call exit@plt
205	
206	.section .rodata
207	# length of .L._errOutOfBounds_str0
208		.int 42
209	.L._errOutOfBounds_str0:
210		.asciz "fatal error: array index %d out of bounds\n"
211	.text
212	_errOutOfBounds:
213		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
214		and rsp, -16
215		lea rdi, [rip + .L._errOutOfBounds_str0]
216		# on x86, al represents the number of SIMD registers used as variadic arguments
217		mov al, 0
218		call printf@plt
219		mov rdi, 0
220		call fflush@plt
221		mov dil, -1
222		call exit@plt
===========================================================
-- Finished

