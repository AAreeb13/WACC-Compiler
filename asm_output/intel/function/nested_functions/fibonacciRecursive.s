./valid/function/nested_functions/fibonacciRecursive.wacc
calling the reference compiler on ./valid/function/nested_functions/fibonacciRecursive.wacc
-- Test: fibonacciRecursive.wacc

-- Uploaded file: 
---------------------------------------------------------------
# recursive calculation of the first 20 fibonacci numbers

# Output:
# The first 20 fibonacci numbers are:
# 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181...
#

# Program:

begin
  int fibonacci(int n, bool toPrint) is
    if n <= 1
    then
      return n
    else
      skip
    fi ;
    int f1 = call fibonacci(n - 1, toPrint) ;
    if toPrint
    then
      print f1 ;
      print ", "
    else
      skip
    fi ;
    int f2 = call fibonacci(n - 2, false) ;
    return f1 + f2
  end

  println "The first 20 fibonacci numbers are:" ;
  print "0, " ;
  int result = call fibonacci(19, true) ;
  print result ;
  println "..."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciRecursive.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 2
5	.L.str0:
6		.asciz ", "
7	# length of .L.str1
8		.int 35
9	.L.str1:
10		.asciz "The first 20 fibonacci numbers are:"
11	# length of .L.str2
12		.int 3
13	.L.str2:
14		.asciz "0, "
15	# length of .L.str3
16		.int 3
17	.L.str3:
18		.asciz "..."
19	.text
20	main:
21		push rbp
22		# push {rbx, r12}
23		sub rsp, 16
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov rbp, rsp
27		# Stack pointer unchanged, no stack allocated variables
28		# Stack pointer unchanged, no stack allocated arguments
29		lea rax, [rip + .L.str1]
30		push rax
31		pop rax
32		mov rax, rax
33		mov rdi, rax
34		# statement primitives do not return results (but will clobber r0/rax)
35		call _prints
36		call _println
37		# Stack pointer unchanged, no stack allocated arguments
38		lea rax, [rip + .L.str2]
39		push rax
40		pop rax
41		mov rax, rax
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		# Stack pointer unchanged, no stack allocated arguments
46		mov rax, 19
47		mov rdi, rax
48		mov rax, 1
49		mov rsi, rax
50		call wacc_fibonacci
51		mov r11, rax
52		# Stack pointer unchanged, no stack allocated arguments
53		mov rax, r11
54		mov r12, rax
55		# Stack pointer unchanged, no stack allocated arguments
56		mov rax, r12
57		mov rdi, rax
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _printi
60		# Stack pointer unchanged, no stack allocated arguments
61		lea rax, [rip + .L.str3]
62		push rax
63		pop rax
64		mov rax, rax
65		mov rdi, rax
66		# statement primitives do not return results (but will clobber r0/rax)
67		call _prints
68		call _println
69		# Stack pointer unchanged, no stack allocated variables
70		mov rax, 0
71		# pop {rbx, r12}
72		mov rbx, qword ptr [rsp]
73		mov r12, qword ptr [rsp + 8]
74		add rsp, 16
75		pop rbp
76		ret
77	
78	wacc_fibonacci:
79		push rbp
80		# push {r12, r13}
81		sub rsp, 16
82		mov qword ptr [rsp], r12
83		mov qword ptr [rsp + 8], r13
84		mov rbp, rsp
85		# Stack pointer unchanged, no stack allocated variables
86		cmp rdi, 1
87		jle .L0
88		jmp .L1
89	.L0:
90		mov rax, rdi
91		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
92		mov rsp, rbp
93		# pop {r12, r13}
94		mov r12, qword ptr [rsp]
95		mov r13, qword ptr [rsp + 8]
96		add rsp, 16
97		pop rbp
98		ret
99	.L1:
100		# push {rsi, rdi}
101		sub rsp, 16
102		mov qword ptr [rsp], rsi
103		mov qword ptr [rsp + 8], rdi
104		# Set up R11 as a temporary second base pointer for the caller saved things
105		mov r11, rsp
106		# Stack pointer unchanged, no stack allocated arguments
107		mov eax, edi
108		sub eax, 1
109		jo _errOverflow
110		movsx rax, eax
111		push rax
112		pop rax
113		mov rax, rax
114		mov rdi, rax
115		mov rax, rsi
116		mov rsi, rax
117		call wacc_fibonacci
118		mov r11, rax
119		# Stack pointer unchanged, no stack allocated arguments
120		# pop {rsi, rdi}
121		mov rsi, qword ptr [rsp]
122		mov rdi, qword ptr [rsp + 8]
123		add rsp, 16
124		mov rax, r11
125		mov r12, rax
126		cmp rsi, 1
127		je .L2
128		jmp .L3
129	.L2:
130		# push {rsi, rdi}
131		sub rsp, 16
132		mov qword ptr [rsp], rsi
133		mov qword ptr [rsp + 8], rdi
134		# Set up R11 as a temporary second base pointer for the caller saved things
135		mov r11, rsp
136		# Stack pointer unchanged, no stack allocated arguments
137		mov rax, r12
138		mov rdi, rax
139		# statement primitives do not return results (but will clobber r0/rax)
140		call _printi
141		# pop {rsi, rdi}
142		mov rsi, qword ptr [rsp]
143		mov rdi, qword ptr [rsp + 8]
144		add rsp, 16
145		# push {rsi, rdi}
146		sub rsp, 16
147		mov qword ptr [rsp], rsi
148		mov qword ptr [rsp + 8], rdi
149		# Set up R11 as a temporary second base pointer for the caller saved things
150		mov r11, rsp
151		# Stack pointer unchanged, no stack allocated arguments
152		lea rax, [rip + .L.str0]
153		push rax
154		pop rax
155		mov rax, rax
156		mov rdi, rax
157		# statement primitives do not return results (but will clobber r0/rax)
158		call _prints
159		# pop {rsi, rdi}
160		mov rsi, qword ptr [rsp]
161		mov rdi, qword ptr [rsp + 8]
162		add rsp, 16
163	.L3:
164		# push {rsi, rdi}
165		sub rsp, 16
166		mov qword ptr [rsp], rsi
167		mov qword ptr [rsp + 8], rdi
168		# Set up R11 as a temporary second base pointer for the caller saved things
169		mov r11, rsp
170		# Stack pointer unchanged, no stack allocated arguments
171		mov eax, edi
172		sub eax, 2
173		jo _errOverflow
174		movsx rax, eax
175		push rax
176		pop rax
177		mov rax, rax
178		mov rdi, rax
179		mov rax, 0
180		mov rsi, rax
181		call wacc_fibonacci
182		mov r11, rax
183		# Stack pointer unchanged, no stack allocated arguments
184		# pop {rsi, rdi}
185		mov rsi, qword ptr [rsp]
186		mov rdi, qword ptr [rsp + 8]
187		add rsp, 16
188		mov rax, r11
189		mov r13, rax
190		mov eax, r12d
191		add eax, r13d
192		jo _errOverflow
193		movsx rax, eax
194		push rax
195		pop rax
196		mov rax, rax
197		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
198		mov rsp, rbp
199		# pop {r12, r13}
200		mov r12, qword ptr [rsp]
201		mov r13, qword ptr [rsp + 8]
202		add rsp, 16
203		pop rbp
204		ret
205		# Stack pointer unchanged, no stack allocated variables
206		# 'ere be dragons: this is 100% dead code, functions always end in returns!
207	
208	.section .rodata
209	# length of .L._prints_str0
210		.int 4
211	.L._prints_str0:
212		.asciz "%.*s"
213	.text
214	_prints:
215		push rbp
216		mov rbp, rsp
217		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
218		and rsp, -16
219		mov rdx, rdi
220		mov esi, dword ptr [rdi - 4]
221		lea rdi, [rip + .L._prints_str0]
222		# on x86, al represents the number of SIMD registers used as variadic arguments
223		mov al, 0
224		call printf@plt
225		mov rdi, 0
226		call fflush@plt
227		mov rsp, rbp
228		pop rbp
229		ret
230	
231	.section .rodata
232	# length of .L._printi_str0
233		.int 2
234	.L._printi_str0:
235		.asciz "%d"
236	.text
237	_printi:
238		push rbp
239		mov rbp, rsp
240		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
241		and rsp, -16
242		mov esi, edi
243		lea rdi, [rip + .L._printi_str0]
244		# on x86, al represents the number of SIMD registers used as variadic arguments
245		mov al, 0
246		call printf@plt
247		mov rdi, 0
248		call fflush@plt
249		mov rsp, rbp
250		pop rbp
251		ret
252	
253	.section .rodata
254	# length of .L._println_str0
255		.int 0
256	.L._println_str0:
257		.asciz ""
258	.text
259	_println:
260		push rbp
261		mov rbp, rsp
262		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
263		and rsp, -16
264		lea rdi, [rip + .L._println_str0]
265		call puts@plt
266		mov rdi, 0
267		call fflush@plt
268		mov rsp, rbp
269		pop rbp
270		ret
271	
272	_exit:
273		push rbp
274		mov rbp, rsp
275		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
276		and rsp, -16
277		call exit@plt
278		mov rsp, rbp
279		pop rbp
280		ret
281	
282	.section .rodata
283	# length of .L._errOverflow_str0
284		.int 52
285	.L._errOverflow_str0:
286		.asciz "fatal error: integer overflow or underflow occurred\n"
287	.text
288	_errOverflow:
289		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
290		and rsp, -16
291		lea rdi, [rip + .L._errOverflow_str0]
292		call _prints
293		mov dil, -1
294		call exit@plt
===========================================================
-- Finished

