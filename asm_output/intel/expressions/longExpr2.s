./valid/expressions/longExpr2.wacc
calling the reference compiler on ./valid/expressions/longExpr2.wacc
-- Test: longExpr2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 10

begin

  int x = (2 + 3 + 2 + 1 + 1 + 1) - (1 + 2) * (3 - 4 / 6) / ( 2 * (18 - 17) + (3 * 4 / 4 + 6));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr2.s contents are:
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
12		mov eax, 2
13		add eax, 3
14		jo _errOverflow
15		movsx rax, eax
16		push rax
17		pop rax
18		mov eax, eax
19		add eax, 2
20		jo _errOverflow
21		movsx rax, eax
22		push rax
23		pop rax
24		mov eax, eax
25		add eax, 1
26		jo _errOverflow
27		movsx rax, eax
28		push rax
29		pop rax
30		mov eax, eax
31		add eax, 1
32		jo _errOverflow
33		movsx rax, eax
34		push rax
35		pop rax
36		mov eax, eax
37		add eax, 1
38		jo _errOverflow
39		movsx rax, eax
40		push rax
41		mov eax, 1
42		add eax, 2
43		jo _errOverflow
44		movsx rax, eax
45		push rax
46		mov rbx, 6
47		mov eax, 4
48		cmp ebx, 0
49		je _errDivZero
50		# sign extend EAX into EDX
51		cdq
52		idiv ebx
53		mov eax, eax
54		mov eax, eax
55		movsx rax, eax
56		push rax
57		pop rbx
58		mov eax, 3
59		sub eax, ebx
60		jo _errOverflow
61		movsx rax, eax
62		push rax
63		pop rbx
64		pop rax
65		mov eax, eax
66		imul eax, ebx
67		jo _errOverflow
68		movsx rax, eax
69		push rax
70		mov eax, 18
71		sub eax, 17
72		jo _errOverflow
73		movsx rax, eax
74		push rax
75		pop rbx
76		mov rax, 2
77		mov eax, eax
78		imul eax, ebx
79		jo _errOverflow
80		movsx rax, eax
81		push rax
82		mov rax, 3
83		imul eax, eax, 4
84		jo _errOverflow
85		movsx rax, eax
86		push rax
87		mov rbx, 4
88		pop rax
89		mov eax, eax
90		cmp ebx, 0
91		je _errDivZero
92		# sign extend EAX into EDX
93		cdq
94		idiv ebx
95		mov eax, eax
96		mov eax, eax
97		movsx rax, eax
98		push rax
99		pop rax
100		mov eax, eax
101		add eax, 6
102		jo _errOverflow
103		movsx rax, eax
104		push rax
105		pop rbx
106		pop rax
107		mov eax, eax
108		add eax, ebx
109		jo _errOverflow
110		movsx rax, eax
111		push rax
112		pop rbx
113		pop rax
114		mov eax, eax
115		cmp ebx, 0
116		je _errDivZero
117		# sign extend EAX into EDX
118		cdq
119		idiv ebx
120		mov eax, eax
121		mov eax, eax
122		movsx rax, eax
123		push rax
124		pop rbx
125		pop rax
126		mov eax, eax
127		sub eax, ebx
128		jo _errOverflow
129		movsx rax, eax
130		push rax
131		pop rax
132		mov rax, rax
133		mov r12, rax
134		# Stack pointer unchanged, no stack allocated arguments
135		mov rax, r12
136		mov rdi, rax
137		# statement primitives do not return results (but will clobber r0/rax)
138		call _exit
139		# Stack pointer unchanged, no stack allocated variables
140		mov rax, 0
141		# pop {rbx, r12}
142		mov rbx, qword ptr [rsp]
143		mov r12, qword ptr [rsp + 8]
144		add rsp, 16
145		pop rbp
146		ret
147	
148	.section .rodata
149	# length of .L._prints_str0
150		.int 4
151	.L._prints_str0:
152		.asciz "%.*s"
153	.text
154	_prints:
155		push rbp
156		mov rbp, rsp
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		and rsp, -16
159		mov rdx, rdi
160		mov esi, dword ptr [rdi - 4]
161		lea rdi, [rip + .L._prints_str0]
162		# on x86, al represents the number of SIMD registers used as variadic arguments
163		mov al, 0
164		call printf@plt
165		mov rdi, 0
166		call fflush@plt
167		mov rsp, rbp
168		pop rbp
169		ret
170	
171	_exit:
172		push rbp
173		mov rbp, rsp
174		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
175		and rsp, -16
176		call exit@plt
177		mov rsp, rbp
178		pop rbp
179		ret
180	
181	.section .rodata
182	# length of .L._errDivZero_str0
183		.int 40
184	.L._errDivZero_str0:
185		.asciz "fatal error: division or modulo by zero\n"
186	.text
187	_errDivZero:
188		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
189		and rsp, -16
190		lea rdi, [rip + .L._errDivZero_str0]
191		call _prints
192		mov dil, -1
193		call exit@plt
194	
195	.section .rodata
196	# length of .L._errOverflow_str0
197		.int 52
198	.L._errOverflow_str0:
199		.asciz "fatal error: integer overflow or underflow occurred\n"
200	.text
201	_errOverflow:
202		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
203		and rsp, -16
204		lea rdi, [rip + .L._errOverflow_str0]
205		call _prints
206		mov dil, -1
207		call exit@plt
===========================================================
-- Finished

