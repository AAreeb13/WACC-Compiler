./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
-- Test: arrayOutOfBoundsWrite.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt write out of array bounds (gods, this really should not work!)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	a[5] = 100 ;
  println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBoundsWrite.s contents are:
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
13		# 3 element array
14		mov edi, 16
15		call _malloc
16		mov r11, rax
17		# array pointers are shifted forwards by 4 bytes (to account for size)
18		mov r11, r11
19		add r11, 4
20		mov rax, 3
21		mov dword ptr [r11 - 4], eax
22		mov rax, 1
23		mov dword ptr [r11], eax
24		mov rax, 2
25		mov dword ptr [r11 + 4], eax
26		mov rax, 3
27		mov dword ptr [r11 + 8], eax
28		mov rax, r11
29		mov r12, rax
30		# 4 element array
31		mov edi, 20
32		call _malloc
33		mov r11, rax
34		# array pointers are shifted forwards by 4 bytes (to account for size)
35		mov r11, r11
36		add r11, 4
37		mov rax, 4
38		mov dword ptr [r11 - 4], eax
39		mov rax, 43
40		mov dword ptr [r11], eax
41		mov rax, 2
42		mov dword ptr [r11 + 4], eax
43		mov rax, 18
44		mov dword ptr [r11 + 8], eax
45		mov rax, 1
46		mov dword ptr [r11 + 12], eax
47		mov rax, r11
48		mov r13, rax
49		mov r10d, 5
50		mov rax, 100
51		mov r9, r13
52		call _arrStore4
53		# Stack pointer unchanged, no stack allocated arguments
54		mov r10d, 5
55		mov r9, r13
56		call _arrLoad4
57		mov eax, r9d
58		mov rax, rax
59		mov rdi, rax
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _printi
62		call _println
63		# Stack pointer unchanged, no stack allocated variables
64		mov rax, 0
65		# pop {rbx, r12, r13}
66		mov rbx, qword ptr [rsp]
67		mov r12, qword ptr [rsp + 8]
68		mov r13, qword ptr [rsp + 16]
69		add rsp, 24
70		pop rbp
71		ret
72	
73	.section .rodata
74	# length of .L._prints_str0
75		.int 4
76	.L._prints_str0:
77		.asciz "%.*s"
78	.text
79	_prints:
80		push rbp
81		mov rbp, rsp
82		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
83		and rsp, -16
84		mov rdx, rdi
85		mov esi, dword ptr [rdi - 4]
86		lea rdi, [rip + .L._prints_str0]
87		# on x86, al represents the number of SIMD registers used as variadic arguments
88		mov al, 0
89		call printf@plt
90		mov rdi, 0
91		call fflush@plt
92		mov rsp, rbp
93		pop rbp
94		ret
95	
96	_malloc:
97		push rbp
98		mov rbp, rsp
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		and rsp, -16
101		call malloc@plt
102		cmp rax, 0
103		je _errOutOfMemory
104		mov rsp, rbp
105		pop rbp
106		ret
107	
108	.section .rodata
109	# length of .L._printi_str0
110		.int 2
111	.L._printi_str0:
112		.asciz "%d"
113	.text
114	_printi:
115		push rbp
116		mov rbp, rsp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		and rsp, -16
119		mov esi, edi
120		lea rdi, [rip + .L._printi_str0]
121		# on x86, al represents the number of SIMD registers used as variadic arguments
122		mov al, 0
123		call printf@plt
124		mov rdi, 0
125		call fflush@plt
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
159	_arrStore4:
160		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
161		push rbx
162		cmp r10d, 0
163		cmovl esi, r10d
164		jl _errOutOfBounds
165		mov ebx, dword ptr [r9 - 4]
166		cmp r10d, ebx
167		cmovge esi, r10d
168		jge _errOutOfBounds
169		mov dword ptr [r9 + 4*r10], eax
170		pop rbx
171		ret
172	
173	_arrLoad4:
174		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
175		push rbx
176		cmp r10d, 0
177		cmovl esi, r10d
178		jl _errOutOfBounds
179		mov ebx, dword ptr [r9 - 4]
180		cmp r10d, ebx
181		cmovge esi, r10d
182		jge _errOutOfBounds
183		movsx r9, dword ptr [r9 + 4*r10]
184		pop rbx
185		ret
186	
187	.section .rodata
188	# length of .L._errOutOfMemory_str0
189		.int 27
190	.L._errOutOfMemory_str0:
191		.asciz "fatal error: out of memory\n"
192	.text
193	_errOutOfMemory:
194		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
195		and rsp, -16
196		lea rdi, [rip + .L._errOutOfMemory_str0]
197		call _prints
198		mov dil, -1
199		call exit@plt
200	
201	.section .rodata
202	# length of .L._errOutOfBounds_str0
203		.int 42
204	.L._errOutOfBounds_str0:
205		.asciz "fatal error: array index %d out of bounds\n"
206	.text
207	_errOutOfBounds:
208		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
209		and rsp, -16
210		lea rdi, [rip + .L._errOutOfBounds_str0]
211		# on x86, al represents the number of SIMD registers used as variadic arguments
212		mov al, 0
213		call printf@plt
214		mov rdi, 0
215		call fflush@plt
216		mov dil, -1
217		call exit@plt
===========================================================
-- Finished

