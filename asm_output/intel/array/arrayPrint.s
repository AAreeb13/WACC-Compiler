./valid/array/arrayPrint.wacc
calling the reference compiler on ./valid/array/arrayPrint.wacc
-- Test: arrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the contents of a simple array

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] ;
  int i = 0 ;
  print a ;
  print " = {" ;
  i = 0 ;
  while i < 10
  do
    print a[i] ;
    if i < 9
    then
      print ", "
    else
      skip
    fi ;
    i = i + 1
  done ;
  println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayPrint.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 4
5	.L.str0:
6		.asciz " = {"
7	# length of .L.str1
8		.int 2
9	.L.str1:
10		.asciz ", "
11	# length of .L.str2
12		.int 1
13	.L.str2:
14		.asciz "}"
15	.text
16	main:
17		push rbp
18		# push {rbx, r12, r13}
19		sub rsp, 24
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov rbp, rsp
24		# Stack pointer unchanged, no stack allocated variables
25		# 10 element array
26		mov edi, 44
27		call _malloc
28		mov r11, rax
29		# array pointers are shifted forwards by 4 bytes (to account for size)
30		mov r11, r11
31		add r11, 4
32		mov rax, 10
33		mov dword ptr [r11 - 4], eax
34		mov rax, 0
35		mov dword ptr [r11], eax
36		mov rax, 1
37		mov dword ptr [r11 + 4], eax
38		mov rax, 2
39		mov dword ptr [r11 + 8], eax
40		mov rax, 3
41		mov dword ptr [r11 + 12], eax
42		mov rax, 4
43		mov dword ptr [r11 + 16], eax
44		mov rax, 5
45		mov dword ptr [r11 + 20], eax
46		mov rax, 6
47		mov dword ptr [r11 + 24], eax
48		mov rax, 7
49		mov dword ptr [r11 + 28], eax
50		mov rax, 8
51		mov dword ptr [r11 + 32], eax
52		mov rax, 9
53		mov dword ptr [r11 + 36], eax
54		mov rax, r11
55		mov r12, rax
56		mov rax, 0
57		mov r13, rax
58		# Stack pointer unchanged, no stack allocated arguments
59		mov rax, r12
60		mov rdi, rax
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _printp
63		# Stack pointer unchanged, no stack allocated arguments
64		lea rax, [rip + .L.str0]
65		push rax
66		pop rax
67		mov rax, rax
68		mov rdi, rax
69		# statement primitives do not return results (but will clobber r0/rax)
70		call _prints
71		mov rax, 0
72		mov r13, rax
73		jmp .L0
74	.L1:
75		# Stack pointer unchanged, no stack allocated arguments
76		mov r10d, r13d
77		mov r9, r12
78		call _arrLoad4
79		mov eax, r9d
80		mov rax, rax
81		mov rdi, rax
82		# statement primitives do not return results (but will clobber r0/rax)
83		call _printi
84		cmp r13, 9
85		jl .L2
86		jmp .L3
87	.L2:
88		# Stack pointer unchanged, no stack allocated arguments
89		lea rax, [rip + .L.str1]
90		push rax
91		pop rax
92		mov rax, rax
93		mov rdi, rax
94		# statement primitives do not return results (but will clobber r0/rax)
95		call _prints
96	.L3:
97		mov eax, r13d
98		add eax, 1
99		jo _errOverflow
100		movsx rax, eax
101		push rax
102		pop rax
103		mov rax, rax
104		mov r13, rax
105	.L0:
106		cmp r13, 10
107		jl .L1
108		# Stack pointer unchanged, no stack allocated arguments
109		lea rax, [rip + .L.str2]
110		push rax
111		pop rax
112		mov rax, rax
113		mov rdi, rax
114		# statement primitives do not return results (but will clobber r0/rax)
115		call _prints
116		call _println
117		# Stack pointer unchanged, no stack allocated variables
118		mov rax, 0
119		# pop {rbx, r12, r13}
120		mov rbx, qword ptr [rsp]
121		mov r12, qword ptr [rsp + 8]
122		mov r13, qword ptr [rsp + 16]
123		add rsp, 24
124		pop rbp
125		ret
126	
127	.section .rodata
128	# length of .L._prints_str0
129		.int 4
130	.L._prints_str0:
131		.asciz "%.*s"
132	.text
133	_prints:
134		push rbp
135		mov rbp, rsp
136		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
137		and rsp, -16
138		mov rdx, rdi
139		mov esi, dword ptr [rdi - 4]
140		lea rdi, [rip + .L._prints_str0]
141		# on x86, al represents the number of SIMD registers used as variadic arguments
142		mov al, 0
143		call printf@plt
144		mov rdi, 0
145		call fflush@plt
146		mov rsp, rbp
147		pop rbp
148		ret
149	
150	.section .rodata
151	# length of .L._printp_str0
152		.int 2
153	.L._printp_str0:
154		.asciz "%p"
155	.text
156	_printp:
157		push rbp
158		mov rbp, rsp
159		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
160		and rsp, -16
161		mov rsi, rdi
162		lea rdi, [rip + .L._printp_str0]
163		# on x86, al represents the number of SIMD registers used as variadic arguments
164		mov al, 0
165		call printf@plt
166		mov rdi, 0
167		call fflush@plt
168		mov rsp, rbp
169		pop rbp
170		ret
171	
172	_malloc:
173		push rbp
174		mov rbp, rsp
175		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
176		and rsp, -16
177		call malloc@plt
178		cmp rax, 0
179		je _errOutOfMemory
180		mov rsp, rbp
181		pop rbp
182		ret
183	
184	.section .rodata
185	# length of .L._printi_str0
186		.int 2
187	.L._printi_str0:
188		.asciz "%d"
189	.text
190	_printi:
191		push rbp
192		mov rbp, rsp
193		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
194		and rsp, -16
195		mov esi, edi
196		lea rdi, [rip + .L._printi_str0]
197		# on x86, al represents the number of SIMD registers used as variadic arguments
198		mov al, 0
199		call printf@plt
200		mov rdi, 0
201		call fflush@plt
202		mov rsp, rbp
203		pop rbp
204		ret
205	
206	.section .rodata
207	# length of .L._println_str0
208		.int 0
209	.L._println_str0:
210		.asciz ""
211	.text
212	_println:
213		push rbp
214		mov rbp, rsp
215		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
216		and rsp, -16
217		lea rdi, [rip + .L._println_str0]
218		call puts@plt
219		mov rdi, 0
220		call fflush@plt
221		mov rsp, rbp
222		pop rbp
223		ret
224	
225	_exit:
226		push rbp
227		mov rbp, rsp
228		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
229		and rsp, -16
230		call exit@plt
231		mov rsp, rbp
232		pop rbp
233		ret
234	
235	_arrLoad4:
236		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
237		push rbx
238		cmp r10d, 0
239		cmovl esi, r10d
240		jl _errOutOfBounds
241		mov ebx, dword ptr [r9 - 4]
242		cmp r10d, ebx
243		cmovge esi, r10d
244		jge _errOutOfBounds
245		movsx r9, dword ptr [r9 + 4*r10]
246		pop rbx
247		ret
248	
249	.section .rodata
250	# length of .L._errOutOfMemory_str0
251		.int 27
252	.L._errOutOfMemory_str0:
253		.asciz "fatal error: out of memory\n"
254	.text
255	_errOutOfMemory:
256		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
257		and rsp, -16
258		lea rdi, [rip + .L._errOutOfMemory_str0]
259		call _prints
260		mov dil, -1
261		call exit@plt
262	
263	.section .rodata
264	# length of .L._errOutOfBounds_str0
265		.int 42
266	.L._errOutOfBounds_str0:
267		.asciz "fatal error: array index %d out of bounds\n"
268	.text
269	_errOutOfBounds:
270		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
271		and rsp, -16
272		lea rdi, [rip + .L._errOutOfBounds_str0]
273		# on x86, al represents the number of SIMD registers used as variadic arguments
274		mov al, 0
275		call printf@plt
276		mov rdi, 0
277		call fflush@plt
278		mov dil, -1
279		call exit@plt
280	
281	.section .rodata
282	# length of .L._errOverflow_str0
283		.int 52
284	.L._errOverflow_str0:
285		.asciz "fatal error: integer overflow or underflow occurred\n"
286	.text
287	_errOverflow:
288		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
289		and rsp, -16
290		lea rdi, [rip + .L._errOverflow_str0]
291		call _prints
292		mov dil, -1
293		call exit@plt
===========================================================
-- Finished

