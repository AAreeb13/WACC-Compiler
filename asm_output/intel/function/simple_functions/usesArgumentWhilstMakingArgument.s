./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
calling the reference compiler on ./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
-- Test: usesArgumentWhilstMakingArgument.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 12
# -4
# 32
#

# Program:

begin
  int f(int u, int v) is
    int x = call g(u + v, u - v, u * v) ;
    return x
  end

  int g(int x, int y, int z) is
    println(x) ;
    println(y) ;
    println(z) ;
    return 0
  end

  int r = call f(4, 8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
usesArgumentWhilstMakingArgument.s contents are:
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
12		# Stack pointer unchanged, no stack allocated arguments
13		mov rax, 4
14		mov rdi, rax
15		mov rax, 8
16		mov rsi, rax
17		call wacc_f
18		mov r11, rax
19		# Stack pointer unchanged, no stack allocated arguments
20		mov rax, r11
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated variables
23		mov rax, 0
24		# pop {rbx, r12}
25		mov rbx, qword ptr [rsp]
26		mov r12, qword ptr [rsp + 8]
27		add rsp, 16
28		pop rbp
29		ret
30	
31	wacc_f:
32		push rbp
33		push r12
34		mov rbp, rsp
35		# Stack pointer unchanged, no stack allocated variables
36		# push {rsi, rdi}
37		sub rsp, 16
38		mov qword ptr [rsp], rsi
39		mov qword ptr [rsp + 8], rdi
40		# Set up R11 as a temporary second base pointer for the caller saved things
41		mov r11, rsp
42		# Stack pointer unchanged, no stack allocated arguments
43		mov eax, edi
44		add eax, esi
45		jo _errOverflow
46		movsx rax, eax
47		push rax
48		pop rax
49		mov rax, rax
50		mov rdi, rax
51		mov rax, qword ptr [r11 + 8]
52		mov eax, eax
53		sub eax, esi
54		jo _errOverflow
55		movsx rax, eax
56		push rax
57		pop rax
58		mov rax, rax
59		mov rsi, rax
60		mov rbx, qword ptr [r11]
61		mov rax, qword ptr [r11 + 8]
62		mov eax, eax
63		imul eax, ebx
64		jo _errOverflow
65		movsx rax, eax
66		push rax
67		pop rax
68		mov rax, rax
69		mov rdx, rax
70		call wacc_g
71		mov r11, rax
72		# Stack pointer unchanged, no stack allocated arguments
73		# pop {rsi, rdi}
74		mov rsi, qword ptr [rsp]
75		mov rdi, qword ptr [rsp + 8]
76		add rsp, 16
77		mov rax, r11
78		mov r12, rax
79		mov rax, r12
80		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
81		mov rsp, rbp
82		pop r12
83		pop rbp
84		ret
85		# Stack pointer unchanged, no stack allocated variables
86		# 'ere be dragons: this is 100% dead code, functions always end in returns!
87	
88	wacc_g:
89		push rbp
90		mov rbp, rsp
91		# push {rdx, rsi, rdi}
92		sub rsp, 24
93		mov qword ptr [rsp], rdx
94		mov qword ptr [rsp + 8], rsi
95		mov qword ptr [rsp + 16], rdi
96		# Set up R11 as a temporary second base pointer for the caller saved things
97		mov r11, rsp
98		# Stack pointer unchanged, no stack allocated arguments
99		mov rax, rdi
100		mov rdi, rax
101		# statement primitives do not return results (but will clobber r0/rax)
102		call _printi
103		call _println
104		# pop {rdx, rsi, rdi}
105		mov rdx, qword ptr [rsp]
106		mov rsi, qword ptr [rsp + 8]
107		mov rdi, qword ptr [rsp + 16]
108		add rsp, 24
109		# push {rdx, rsi, rdi}
110		sub rsp, 24
111		mov qword ptr [rsp], rdx
112		mov qword ptr [rsp + 8], rsi
113		mov qword ptr [rsp + 16], rdi
114		# Set up R11 as a temporary second base pointer for the caller saved things
115		mov r11, rsp
116		# Stack pointer unchanged, no stack allocated arguments
117		mov rax, rsi
118		mov rdi, rax
119		# statement primitives do not return results (but will clobber r0/rax)
120		call _printi
121		call _println
122		# pop {rdx, rsi, rdi}
123		mov rdx, qword ptr [rsp]
124		mov rsi, qword ptr [rsp + 8]
125		mov rdi, qword ptr [rsp + 16]
126		add rsp, 24
127		# push {rdx, rsi, rdi}
128		sub rsp, 24
129		mov qword ptr [rsp], rdx
130		mov qword ptr [rsp + 8], rsi
131		mov qword ptr [rsp + 16], rdi
132		# Set up R11 as a temporary second base pointer for the caller saved things
133		mov r11, rsp
134		# Stack pointer unchanged, no stack allocated arguments
135		mov rax, rdx
136		mov rdi, rax
137		# statement primitives do not return results (but will clobber r0/rax)
138		call _printi
139		call _println
140		# pop {rdx, rsi, rdi}
141		mov rdx, qword ptr [rsp]
142		mov rsi, qword ptr [rsp + 8]
143		mov rdi, qword ptr [rsp + 16]
144		add rsp, 24
145		mov rax, 0
146		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
147		mov rsp, rbp
148		pop rbp
149		ret
150		# 'ere be dragons: this is 100% dead code, functions always end in returns!
151	
152	.section .rodata
153	# length of .L._prints_str0
154		.int 4
155	.L._prints_str0:
156		.asciz "%.*s"
157	.text
158	_prints:
159		push rbp
160		mov rbp, rsp
161		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
162		and rsp, -16
163		mov rdx, rdi
164		mov esi, dword ptr [rdi - 4]
165		lea rdi, [rip + .L._prints_str0]
166		# on x86, al represents the number of SIMD registers used as variadic arguments
167		mov al, 0
168		call printf@plt
169		mov rdi, 0
170		call fflush@plt
171		mov rsp, rbp
172		pop rbp
173		ret
174	
175	.section .rodata
176	# length of .L._printi_str0
177		.int 2
178	.L._printi_str0:
179		.asciz "%d"
180	.text
181	_printi:
182		push rbp
183		mov rbp, rsp
184		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
185		and rsp, -16
186		mov esi, edi
187		lea rdi, [rip + .L._printi_str0]
188		# on x86, al represents the number of SIMD registers used as variadic arguments
189		mov al, 0
190		call printf@plt
191		mov rdi, 0
192		call fflush@plt
193		mov rsp, rbp
194		pop rbp
195		ret
196	
197	.section .rodata
198	# length of .L._println_str0
199		.int 0
200	.L._println_str0:
201		.asciz ""
202	.text
203	_println:
204		push rbp
205		mov rbp, rsp
206		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
207		and rsp, -16
208		lea rdi, [rip + .L._println_str0]
209		call puts@plt
210		mov rdi, 0
211		call fflush@plt
212		mov rsp, rbp
213		pop rbp
214		ret
215	
216	_exit:
217		push rbp
218		mov rbp, rsp
219		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
220		and rsp, -16
221		call exit@plt
222		mov rsp, rbp
223		pop rbp
224		ret
225	
226	.section .rodata
227	# length of .L._errOverflow_str0
228		.int 52
229	.L._errOverflow_str0:
230		.asciz "fatal error: integer overflow or underflow occurred\n"
231	.text
232	_errOverflow:
233		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
234		and rsp, -16
235		lea rdi, [rip + .L._errOverflow_str0]
236		call _prints
237		mov dil, -1
238		call exit@plt
===========================================================
-- Finished

