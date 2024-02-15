./valid/while/fibonacciIterative.wacc
calling the reference compiler on ./valid/while/fibonacciIterative.wacc
-- Test: fibonacciIterative.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iterative calculation of the first 20 fibonacci numbers

# Output:
# The first 20 fibonacci numbers are:
# 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, ...
#

# Program:

begin
  int i = 0 ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  println "The first 20 fibonacci numbers are:" ;
  while i < 20 do
    print f0 ;
    print ", " ;
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    i = i + 1
  done ;
  println "..."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciIterative.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 35
5	.L.str0:
6		.asciz "The first 20 fibonacci numbers are:"
7	# length of .L.str1
8		.int 2
9	.L.str1:
10		.asciz ", "
11	# length of .L.str2
12		.int 3
13	.L.str2:
14		.asciz "..."
15	.text
16	main:
17		push rbp
18		# push {rbx, r12, r13, r14, r15}
19		sub rsp, 40
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov qword ptr [rsp + 24], r14
24		mov qword ptr [rsp + 32], r15
25		mov rbp, rsp
26		# Stack pointer unchanged, no stack allocated variables
27		mov rax, 0
28		mov r12, rax
29		mov rax, 0
30		mov r13, rax
31		mov rax, 1
32		mov r14, rax
33		mov rax, 0
34		mov r15, rax
35		# Stack pointer unchanged, no stack allocated arguments
36		lea rax, [rip + .L.str0]
37		push rax
38		pop rax
39		mov rax, rax
40		mov rdi, rax
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _prints
43		call _println
44		jmp .L0
45	.L1:
46		# Stack pointer unchanged, no stack allocated arguments
47		mov rax, r13
48		mov rdi, rax
49		# statement primitives do not return results (but will clobber r0/rax)
50		call _printi
51		# Stack pointer unchanged, no stack allocated arguments
52		lea rax, [rip + .L.str1]
53		push rax
54		pop rax
55		mov rax, rax
56		mov rdi, rax
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _prints
59		mov rax, r13
60		mov r15, rax
61		mov rax, r14
62		mov r13, rax
63		mov eax, r15d
64		add eax, r14d
65		jo _errOverflow
66		movsx rax, eax
67		push rax
68		pop rax
69		mov rax, rax
70		mov r14, rax
71		mov eax, r12d
72		add eax, 1
73		jo _errOverflow
74		movsx rax, eax
75		push rax
76		pop rax
77		mov rax, rax
78		mov r12, rax
79	.L0:
80		cmp r12, 20
81		jl .L1
82		# Stack pointer unchanged, no stack allocated arguments
83		lea rax, [rip + .L.str2]
84		push rax
85		pop rax
86		mov rax, rax
87		mov rdi, rax
88		# statement primitives do not return results (but will clobber r0/rax)
89		call _prints
90		call _println
91		# Stack pointer unchanged, no stack allocated variables
92		mov rax, 0
93		# pop {rbx, r12, r13, r14, r15}
94		mov rbx, qword ptr [rsp]
95		mov r12, qword ptr [rsp + 8]
96		mov r13, qword ptr [rsp + 16]
97		mov r14, qword ptr [rsp + 24]
98		mov r15, qword ptr [rsp + 32]
99		add rsp, 40
100		pop rbp
101		ret
102	
103	.section .rodata
104	# length of .L._prints_str0
105		.int 4
106	.L._prints_str0:
107		.asciz "%.*s"
108	.text
109	_prints:
110		push rbp
111		mov rbp, rsp
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		and rsp, -16
114		mov rdx, rdi
115		mov esi, dword ptr [rdi - 4]
116		lea rdi, [rip + .L._prints_str0]
117		# on x86, al represents the number of SIMD registers used as variadic arguments
118		mov al, 0
119		call printf@plt
120		mov rdi, 0
121		call fflush@plt
122		mov rsp, rbp
123		pop rbp
124		ret
125	
126	.section .rodata
127	# length of .L._printi_str0
128		.int 2
129	.L._printi_str0:
130		.asciz "%d"
131	.text
132	_printi:
133		push rbp
134		mov rbp, rsp
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		and rsp, -16
137		mov esi, edi
138		lea rdi, [rip + .L._printi_str0]
139		# on x86, al represents the number of SIMD registers used as variadic arguments
140		mov al, 0
141		call printf@plt
142		mov rdi, 0
143		call fflush@plt
144		mov rsp, rbp
145		pop rbp
146		ret
147	
148	.section .rodata
149	# length of .L._println_str0
150		.int 0
151	.L._println_str0:
152		.asciz ""
153	.text
154	_println:
155		push rbp
156		mov rbp, rsp
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		and rsp, -16
159		lea rdi, [rip + .L._println_str0]
160		call puts@plt
161		mov rdi, 0
162		call fflush@plt
163		mov rsp, rbp
164		pop rbp
165		ret
166	
167	_exit:
168		push rbp
169		mov rbp, rsp
170		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
171		and rsp, -16
172		call exit@plt
173		mov rsp, rbp
174		pop rbp
175		ret
176	
177	.section .rodata
178	# length of .L._errOverflow_str0
179		.int 52
180	.L._errOverflow_str0:
181		.asciz "fatal error: integer overflow or underflow occurred\n"
182	.text
183	_errOverflow:
184		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
185		and rsp, -16
186		lea rdi, [rip + .L._errOverflow_str0]
187		call _prints
188		mov dil, -1
189		call exit@plt
===========================================================
-- Finished

