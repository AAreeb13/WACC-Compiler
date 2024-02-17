./valid/function/simple_functions/manyArgumentsChar.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsChar.wacc
-- Test: manyArgumentsChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# A
# b
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  char f(int s, int t, int u, int v, int w, int x, char y, bool z) is
    int i = u + v ;
    int j = w + x ;
    if z
        then return chr (ord y - i * j)
        else return y
    fi
  end

  char r1 = call f(0, 0, 3, 5, 1, 3, 'a', true) ;
  println r1 ;

  char r2 = call f(0, 0, 3, 5, 1, 3, 'b', false) ;
  println r2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsChar.s contents are:
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
13		sub rsp, 2
14		mov rax, 0
15		mov rdi, rax
16		mov rax, 0
17		mov rsi, rax
18		mov rax, 3
19		mov rdx, rax
20		mov rax, 5
21		mov rcx, rax
22		mov rax, 1
23		mov r8, rax
24		mov rax, 3
25		mov r9, rax
26		mov rax, 97
27		mov byte ptr [rsp + 1], al
28		mov rax, 1
29		mov byte ptr [rsp], al
30		call wacc_f
31		mov r11, rax
32		add rsp, 2
33		mov rax, r11
34		mov r12, rax
35		# Stack pointer unchanged, no stack allocated arguments
36		mov rax, r12
37		mov rdi, rax
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printc
40		call _println
41		sub rsp, 2
42		mov rax, 0
43		mov rdi, rax
44		mov rax, 0
45		mov rsi, rax
46		mov rax, 3
47		mov rdx, rax
48		mov rax, 5
49		mov rcx, rax
50		mov rax, 1
51		mov r8, rax
52		mov rax, 3
53		mov r9, rax
54		mov rax, 98
55		mov byte ptr [rsp + 1], al
56		mov rax, 0
57		mov byte ptr [rsp], al
58		call wacc_f
59		mov r11, rax
60		add rsp, 2
61		mov rax, r11
62		mov r13, rax
63		# Stack pointer unchanged, no stack allocated arguments
64		mov rax, r13
65		mov rdi, rax
66		# statement primitives do not return results (but will clobber r0/rax)
67		call _printc
68		call _println
69		# Stack pointer unchanged, no stack allocated variables
70		mov rax, 0
71		# pop {rbx, r12, r13}
72		mov rbx, qword ptr [rsp]
73		mov r12, qword ptr [rsp + 8]
74		mov r13, qword ptr [rsp + 16]
75		add rsp, 24
76		pop rbp
77		ret
78	
79	wacc_f:
80		push rbp
81		# push {r12, r13}
82		sub rsp, 16
83		mov qword ptr [rsp], r12
84		mov qword ptr [rsp + 8], r13
85		mov rbp, rsp
86		# Stack pointer unchanged, no stack allocated variables
87		mov eax, edx
88		add eax, ecx
89		jo _errOverflow
90		movsx rax, eax
91		push rax
92		pop rax
93		mov rax, rax
94		mov r12, rax
95		mov eax, r8d
96		add eax, r9d
97		jo _errOverflow
98		movsx rax, eax
99		push rax
100		pop rax
101		mov rax, rax
102		mov r13, rax
103		movsx rax, byte ptr [rbp + 32]
104		cmp rax, 1
105		je .L0
106		movsx rax, byte ptr [rbp + 33]
107		mov rax, rax
108		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
109		mov rsp, rbp
110		# pop {r12, r13}
111		mov r12, qword ptr [rsp]
112		mov r13, qword ptr [rsp + 8]
113		add rsp, 16
114		pop rbp
115		ret
116		jmp .L1
117	.L0:
118		mov eax, r12d
119		imul eax, r13d
120		jo _errOverflow
121		movsx rax, eax
122		push rax
123		pop rbx
124		movsx rax, byte ptr [rbp + 33]
125		mov eax, eax
126		sub eax, ebx
127		jo _errOverflow
128		movsx rax, eax
129		push rax
130		pop rax
131		mov rax, rax
132		test rax, -128
133		cmovne rsi, rax
134		jne _errBadChar
135		push rax
136		pop rax
137		mov rax, rax
138		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
139		mov rsp, rbp
140		# pop {r12, r13}
141		mov r12, qword ptr [rsp]
142		mov r13, qword ptr [rsp + 8]
143		add rsp, 16
144		pop rbp
145		ret
146	.L1:
147		# Stack pointer unchanged, no stack allocated variables
148		# 'ere be dragons: this is 100% dead code, functions always end in returns!
149	
150	.section .rodata
151	# length of .L._prints_str0
152		.int 4
153	.L._prints_str0:
154		.asciz "%.*s"
155	.text
156	_prints:
157		push rbp
158		mov rbp, rsp
159		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
160		and rsp, -16
161		mov rdx, rdi
162		mov esi, dword ptr [rdi - 4]
163		lea rdi, [rip + .L._prints_str0]
164		# on x86, al represents the number of SIMD registers used as variadic arguments
165		mov al, 0
166		call printf@plt
167		mov rdi, 0
168		call fflush@plt
169		mov rsp, rbp
170		pop rbp
171		ret
172	
173	.section .rodata
174	# length of .L._printc_str0
175		.int 2
176	.L._printc_str0:
177		.asciz "%c"
178	.text
179	_printc:
180		push rbp
181		mov rbp, rsp
182		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
183		and rsp, -16
184		mov sil, dil
185		lea rdi, [rip + .L._printc_str0]
186		# on x86, al represents the number of SIMD registers used as variadic arguments
187		mov al, 0
188		call printf@plt
189		mov rdi, 0
190		call fflush@plt
191		mov rsp, rbp
192		pop rbp
193		ret
194	
195	.section .rodata
196	# length of .L._println_str0
197		.int 0
198	.L._println_str0:
199		.asciz ""
200	.text
201	_println:
202		push rbp
203		mov rbp, rsp
204		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
205		and rsp, -16
206		lea rdi, [rip + .L._println_str0]
207		call puts@plt
208		mov rdi, 0
209		call fflush@plt
210		mov rsp, rbp
211		pop rbp
212		ret
213	
214	_exit:
215		push rbp
216		mov rbp, rsp
217		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
218		and rsp, -16
219		call exit@plt
220		mov rsp, rbp
221		pop rbp
222		ret
223	
224	.section .rodata
225	# length of .L._errBadChar_str0
226		.int 50
227	.L._errBadChar_str0:
228		.asciz "fatal error: int %d is not ascii character 0-127 \n"
229	.text
230	_errBadChar:
231		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
232		and rsp, -16
233		lea rdi, [rip + .L._errBadChar_str0]
234		# on x86, al represents the number of SIMD registers used as variadic arguments
235		mov al, 0
236		call printf@plt
237		mov rdi, 0
238		call fflush@plt
239		mov dil, -1
240		call exit@plt
241	
242	.section .rodata
243	# length of .L._errOverflow_str0
244		.int 52
245	.L._errOverflow_str0:
246		.asciz "fatal error: integer overflow or underflow occurred\n"
247	.text
248	_errOverflow:
249		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
250		and rsp, -16
251		lea rdi, [rip + .L._errOverflow_str0]
252		call _prints
253		mov dil, -1
254		call exit@plt
===========================================================
-- Finished

