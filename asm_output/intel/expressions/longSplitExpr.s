./valid/expressions/longSplitExpr.wacc
calling the reference compiler on ./valid/expressions/longSplitExpr.wacc
-- Test: longSplitExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions with several variables

# Output:

# Exit:
# 153

begin

  int a = 1 + 2 ;
  int b = 3 + 4 ;
  int c = 5 + 6 ;
  int d = 7 + 8 ;
  int e = 9 + 10 ;
  int f = 11 + 12 ;
  int g = 13 + 14 ;
  int h = 15 + 16 ;
  int i = 17 ;
  exit a + b + c + d + e + f + g + h + i

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longSplitExpr.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14, r15}
7		sub rsp, 40
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov qword ptr [rsp + 32], r15
13		mov rbp, rsp
14		# Stack pointer unchanged, no stack allocated variables
15		mov eax, 1
16		add eax, 2
17		jo _errOverflow
18		movsx rax, eax
19		push rax
20		pop rax
21		mov rax, rax
22		mov r12, rax
23		mov eax, 3
24		add eax, 4
25		jo _errOverflow
26		movsx rax, eax
27		push rax
28		pop rax
29		mov rax, rax
30		mov r13, rax
31		mov eax, 5
32		add eax, 6
33		jo _errOverflow
34		movsx rax, eax
35		push rax
36		pop rax
37		mov rax, rax
38		mov r14, rax
39		mov eax, 7
40		add eax, 8
41		jo _errOverflow
42		movsx rax, eax
43		push rax
44		pop rax
45		mov rax, rax
46		mov r15, rax
47		mov eax, 9
48		add eax, 10
49		jo _errOverflow
50		movsx rax, eax
51		push rax
52		pop rax
53		mov rax, rax
54		mov rcx, rax
55		mov eax, 11
56		add eax, 12
57		jo _errOverflow
58		movsx rax, eax
59		push rax
60		pop rax
61		mov rax, rax
62		mov rdx, rax
63		mov eax, 13
64		add eax, 14
65		jo _errOverflow
66		movsx rax, eax
67		push rax
68		pop rax
69		mov rax, rax
70		mov rsi, rax
71		mov eax, 15
72		add eax, 16
73		jo _errOverflow
74		movsx rax, eax
75		push rax
76		pop rax
77		mov rax, rax
78		mov rdi, rax
79		mov rax, 17
80		mov r8, rax
81		# push {rcx, rdx, rsi, rdi, r8}
82		sub rsp, 40
83		mov qword ptr [rsp], rcx
84		mov qword ptr [rsp + 8], rdx
85		mov qword ptr [rsp + 16], rsi
86		mov qword ptr [rsp + 24], rdi
87		mov qword ptr [rsp + 32], r8
88		# Set up R11 as a temporary second base pointer for the caller saved things
89		mov r11, rsp
90		# Stack pointer unchanged, no stack allocated arguments
91		mov eax, r12d
92		add eax, r13d
93		jo _errOverflow
94		movsx rax, eax
95		push rax
96		pop rax
97		mov eax, eax
98		add eax, r14d
99		jo _errOverflow
100		movsx rax, eax
101		push rax
102		pop rax
103		mov eax, eax
104		add eax, r15d
105		jo _errOverflow
106		movsx rax, eax
107		push rax
108		pop rax
109		mov eax, eax
110		add eax, ecx
111		jo _errOverflow
112		movsx rax, eax
113		push rax
114		pop rax
115		mov eax, eax
116		add eax, edx
117		jo _errOverflow
118		movsx rax, eax
119		push rax
120		pop rax
121		mov eax, eax
122		add eax, esi
123		jo _errOverflow
124		movsx rax, eax
125		push rax
126		pop rax
127		mov eax, eax
128		add eax, edi
129		jo _errOverflow
130		movsx rax, eax
131		push rax
132		pop rax
133		mov eax, eax
134		add eax, r8d
135		jo _errOverflow
136		movsx rax, eax
137		push rax
138		pop rax
139		mov rax, rax
140		mov rdi, rax
141		# statement primitives do not return results (but will clobber r0/rax)
142		call _exit
143		# pop {rcx, rdx, rsi, rdi, r8}
144		mov rcx, qword ptr [rsp]
145		mov rdx, qword ptr [rsp + 8]
146		mov rsi, qword ptr [rsp + 16]
147		mov rdi, qword ptr [rsp + 24]
148		mov r8, qword ptr [rsp + 32]
149		add rsp, 40
150		# Stack pointer unchanged, no stack allocated variables
151		mov rax, 0
152		# pop {rbx, r12, r13, r14, r15}
153		mov rbx, qword ptr [rsp]
154		mov r12, qword ptr [rsp + 8]
155		mov r13, qword ptr [rsp + 16]
156		mov r14, qword ptr [rsp + 24]
157		mov r15, qword ptr [rsp + 32]
158		add rsp, 40
159		pop rbp
160		ret
161	
162	.section .rodata
163	# length of .L._prints_str0
164		.int 4
165	.L._prints_str0:
166		.asciz "%.*s"
167	.text
168	_prints:
169		push rbp
170		mov rbp, rsp
171		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
172		and rsp, -16
173		mov rdx, rdi
174		mov esi, dword ptr [rdi - 4]
175		lea rdi, [rip + .L._prints_str0]
176		# on x86, al represents the number of SIMD registers used as variadic arguments
177		mov al, 0
178		call printf@plt
179		mov rdi, 0
180		call fflush@plt
181		mov rsp, rbp
182		pop rbp
183		ret
184	
185	_exit:
186		push rbp
187		mov rbp, rsp
188		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
189		and rsp, -16
190		call exit@plt
191		mov rsp, rbp
192		pop rbp
193		ret
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

