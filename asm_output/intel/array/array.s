./valid/array/array.wacc
calling the reference compiler on ./valid/array/array.wacc
-- Test: array.wacc

-- Uploaded file: 
---------------------------------------------------------------
# moderate complexity array manipulations

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ;
  int i = 0 ;
  while i < 10
  do
    a[i] = i ;
    i = i + 1
  done ;
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
array.s contents are:
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
36		mov rax, 0
37		mov dword ptr [r11 + 4], eax
38		mov rax, 0
39		mov dword ptr [r11 + 8], eax
40		mov rax, 0
41		mov dword ptr [r11 + 12], eax
42		mov rax, 0
43		mov dword ptr [r11 + 16], eax
44		mov rax, 0
45		mov dword ptr [r11 + 20], eax
46		mov rax, 0
47		mov dword ptr [r11 + 24], eax
48		mov rax, 0
49		mov dword ptr [r11 + 28], eax
50		mov rax, 0
51		mov dword ptr [r11 + 32], eax
52		mov rax, 0
53		mov dword ptr [r11 + 36], eax
54		mov rax, r11
55		mov r12, rax
56		mov rax, 0
57		mov r13, rax
58		jmp .L0
59	.L1:
60		mov r10d, r13d
61		mov rax, r13
62		mov r9, r12
63		call _arrStore4
64		mov eax, r13d
65		add eax, 1
66		jo _errOverflow
67		movsx rax, eax
68		push rax
69		pop rax
70		mov rax, rax
71		mov r13, rax
72	.L0:
73		cmp r13, 10
74		jl .L1
75		# Stack pointer unchanged, no stack allocated arguments
76		mov rax, r12
77		mov rdi, rax
78		# statement primitives do not return results (but will clobber r0/rax)
79		call _printp
80		# Stack pointer unchanged, no stack allocated arguments
81		lea rax, [rip + .L.str0]
82		push rax
83		pop rax
84		mov rax, rax
85		mov rdi, rax
86		# statement primitives do not return results (but will clobber r0/rax)
87		call _prints
88		mov rax, 0
89		mov r13, rax
90		jmp .L2
91	.L3:
92		# Stack pointer unchanged, no stack allocated arguments
93		mov r10d, r13d
94		mov r9, r12
95		call _arrLoad4
96		mov eax, r9d
97		mov rax, rax
98		mov rdi, rax
99		# statement primitives do not return results (but will clobber r0/rax)
100		call _printi
101		cmp r13, 9
102		jl .L4
103		jmp .L5
104	.L4:
105		# Stack pointer unchanged, no stack allocated arguments
106		lea rax, [rip + .L.str1]
107		push rax
108		pop rax
109		mov rax, rax
110		mov rdi, rax
111		# statement primitives do not return results (but will clobber r0/rax)
112		call _prints
113	.L5:
114		mov eax, r13d
115		add eax, 1
116		jo _errOverflow
117		movsx rax, eax
118		push rax
119		pop rax
120		mov rax, rax
121		mov r13, rax
122	.L2:
123		cmp r13, 10
124		jl .L3
125		# Stack pointer unchanged, no stack allocated arguments
126		lea rax, [rip + .L.str2]
127		push rax
128		pop rax
129		mov rax, rax
130		mov rdi, rax
131		# statement primitives do not return results (but will clobber r0/rax)
132		call _prints
133		call _println
134		# Stack pointer unchanged, no stack allocated variables
135		mov rax, 0
136		# pop {rbx, r12, r13}
137		mov rbx, qword ptr [rsp]
138		mov r12, qword ptr [rsp + 8]
139		mov r13, qword ptr [rsp + 16]
140		add rsp, 24
141		pop rbp
142		ret
143	
144	.section .rodata
145	# length of .L._prints_str0
146		.int 4
147	.L._prints_str0:
148		.asciz "%.*s"
149	.text
150	_prints:
151		push rbp
152		mov rbp, rsp
153		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
154		and rsp, -16
155		mov rdx, rdi
156		mov esi, dword ptr [rdi - 4]
157		lea rdi, [rip + .L._prints_str0]
158		# on x86, al represents the number of SIMD registers used as variadic arguments
159		mov al, 0
160		call printf@plt
161		mov rdi, 0
162		call fflush@plt
163		mov rsp, rbp
164		pop rbp
165		ret
166	
167	.section .rodata
168	# length of .L._printp_str0
169		.int 2
170	.L._printp_str0:
171		.asciz "%p"
172	.text
173	_printp:
174		push rbp
175		mov rbp, rsp
176		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
177		and rsp, -16
178		mov rsi, rdi
179		lea rdi, [rip + .L._printp_str0]
180		# on x86, al represents the number of SIMD registers used as variadic arguments
181		mov al, 0
182		call printf@plt
183		mov rdi, 0
184		call fflush@plt
185		mov rsp, rbp
186		pop rbp
187		ret
188	
189	_malloc:
190		push rbp
191		mov rbp, rsp
192		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
193		and rsp, -16
194		call malloc@plt
195		cmp rax, 0
196		je _errOutOfMemory
197		mov rsp, rbp
198		pop rbp
199		ret
200	
201	.section .rodata
202	# length of .L._printi_str0
203		.int 2
204	.L._printi_str0:
205		.asciz "%d"
206	.text
207	_printi:
208		push rbp
209		mov rbp, rsp
210		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
211		and rsp, -16
212		mov esi, edi
213		lea rdi, [rip + .L._printi_str0]
214		# on x86, al represents the number of SIMD registers used as variadic arguments
215		mov al, 0
216		call printf@plt
217		mov rdi, 0
218		call fflush@plt
219		mov rsp, rbp
220		pop rbp
221		ret
222	
223	.section .rodata
224	# length of .L._println_str0
225		.int 0
226	.L._println_str0:
227		.asciz ""
228	.text
229	_println:
230		push rbp
231		mov rbp, rsp
232		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
233		and rsp, -16
234		lea rdi, [rip + .L._println_str0]
235		call puts@plt
236		mov rdi, 0
237		call fflush@plt
238		mov rsp, rbp
239		pop rbp
240		ret
241	
242	_exit:
243		push rbp
244		mov rbp, rsp
245		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
246		and rsp, -16
247		call exit@plt
248		mov rsp, rbp
249		pop rbp
250		ret
251	
252	_arrStore4:
253		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
254		push rbx
255		cmp r10d, 0
256		cmovl esi, r10d
257		jl _errOutOfBounds
258		mov ebx, dword ptr [r9 - 4]
259		cmp r10d, ebx
260		cmovge esi, r10d
261		jge _errOutOfBounds
262		mov dword ptr [r9 + 4*r10], eax
263		pop rbx
264		ret
265	
266	_arrLoad4:
267		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
268		push rbx
269		cmp r10d, 0
270		cmovl esi, r10d
271		jl _errOutOfBounds
272		mov ebx, dword ptr [r9 - 4]
273		cmp r10d, ebx
274		cmovge esi, r10d
275		jge _errOutOfBounds
276		movsx r9, dword ptr [r9 + 4*r10]
277		pop rbx
278		ret
279	
280	.section .rodata
281	# length of .L._errOutOfMemory_str0
282		.int 27
283	.L._errOutOfMemory_str0:
284		.asciz "fatal error: out of memory\n"
285	.text
286	_errOutOfMemory:
287		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
288		and rsp, -16
289		lea rdi, [rip + .L._errOutOfMemory_str0]
290		call _prints
291		mov dil, -1
292		call exit@plt
293	
294	.section .rodata
295	# length of .L._errOutOfBounds_str0
296		.int 42
297	.L._errOutOfBounds_str0:
298		.asciz "fatal error: array index %d out of bounds\n"
299	.text
300	_errOutOfBounds:
301		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
302		and rsp, -16
303		lea rdi, [rip + .L._errOutOfBounds_str0]
304		# on x86, al represents the number of SIMD registers used as variadic arguments
305		mov al, 0
306		call printf@plt
307		mov rdi, 0
308		call fflush@plt
309		mov dil, -1
310		call exit@plt
311	
312	.section .rodata
313	# length of .L._errOverflow_str0
314		.int 52
315	.L._errOverflow_str0:
316		.asciz "fatal error: integer overflow or underflow occurred\n"
317	.text
318	_errOverflow:
319		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
320		and rsp, -16
321		lea rdi, [rip + .L._errOverflow_str0]
322		call _prints
323		mov dil, -1
324		call exit@plt
===========================================================
-- Finished

