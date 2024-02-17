./valid/pairs/linkedList.wacc
calling the reference compiler on ./valid/pairs/linkedList.wacc
-- Test: linkedList.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and print a linked list using pairs

# Output:
# list = {1, 2, 4, 11}
#

# Program:

begin
  pair(int, pair) p = newpair(11, null) ;
    pair(int, pair) q = newpair(4, p) ;
    pair(int, pair) r = newpair(2, q) ;
    pair(int, pair) s = newpair(1, r) ;
    print "list = {" ;
    pair(int, pair) x = s ;
    pair(int, pair) y = snd x ;
    int f = 0;
    while y != null do
      f = fst x ;
      print f ;
      print ", " ;
      x = y ;
      y = snd x
    done ;
    f = fst x ;
    print f ;
    println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
linkedList.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 8
5	.L.str0:
6		.asciz "list = {"
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
18		# push {rbx, r12, r13, r14, r15}
19		sub rsp, 40
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov qword ptr [rsp + 24], r14
24		mov qword ptr [rsp + 32], r15
25		mov rbp, rsp
26		# Stack pointer unchanged, no stack allocated variables
27		# push {rcx, rdx, rsi}
28		sub rsp, 24
29		mov qword ptr [rsp], rcx
30		mov qword ptr [rsp + 8], rdx
31		mov qword ptr [rsp + 16], rsi
32		mov edi, 16
33		call _malloc
34		mov r11, rax
35		# pop {rcx, rdx, rsi}
36		mov rcx, qword ptr [rsp]
37		mov rdx, qword ptr [rsp + 8]
38		mov rsi, qword ptr [rsp + 16]
39		add rsp, 24
40		mov rax, 11
41		mov qword ptr [r11], rax
42		mov rax, 0
43		mov qword ptr [r11 + 8], rax
44		mov rax, r11
45		mov r12, rax
46		# push {rcx, rdx, rsi}
47		sub rsp, 24
48		mov qword ptr [rsp], rcx
49		mov qword ptr [rsp + 8], rdx
50		mov qword ptr [rsp + 16], rsi
51		mov edi, 16
52		call _malloc
53		mov r11, rax
54		# pop {rcx, rdx, rsi}
55		mov rcx, qword ptr [rsp]
56		mov rdx, qword ptr [rsp + 8]
57		mov rsi, qword ptr [rsp + 16]
58		add rsp, 24
59		mov rax, 4
60		mov qword ptr [r11], rax
61		mov rax, r12
62		mov qword ptr [r11 + 8], rax
63		mov rax, r11
64		mov r13, rax
65		# push {rcx, rdx, rsi}
66		sub rsp, 24
67		mov qword ptr [rsp], rcx
68		mov qword ptr [rsp + 8], rdx
69		mov qword ptr [rsp + 16], rsi
70		mov edi, 16
71		call _malloc
72		mov r11, rax
73		# pop {rcx, rdx, rsi}
74		mov rcx, qword ptr [rsp]
75		mov rdx, qword ptr [rsp + 8]
76		mov rsi, qword ptr [rsp + 16]
77		add rsp, 24
78		mov rax, 2
79		mov qword ptr [r11], rax
80		mov rax, r13
81		mov qword ptr [r11 + 8], rax
82		mov rax, r11
83		mov r14, rax
84		# push {rcx, rdx, rsi}
85		sub rsp, 24
86		mov qword ptr [rsp], rcx
87		mov qword ptr [rsp + 8], rdx
88		mov qword ptr [rsp + 16], rsi
89		mov edi, 16
90		call _malloc
91		mov r11, rax
92		# pop {rcx, rdx, rsi}
93		mov rcx, qword ptr [rsp]
94		mov rdx, qword ptr [rsp + 8]
95		mov rsi, qword ptr [rsp + 16]
96		add rsp, 24
97		mov rax, 1
98		mov qword ptr [r11], rax
99		mov rax, r14
100		mov qword ptr [r11 + 8], rax
101		mov rax, r11
102		mov r15, rax
103		# push {rcx, rdx, rsi}
104		sub rsp, 24
105		mov qword ptr [rsp], rcx
106		mov qword ptr [rsp + 8], rdx
107		mov qword ptr [rsp + 16], rsi
108		# Set up R11 as a temporary second base pointer for the caller saved things
109		mov r11, rsp
110		# Stack pointer unchanged, no stack allocated arguments
111		lea rax, [rip + .L.str0]
112		push rax
113		pop rax
114		mov rax, rax
115		mov rdi, rax
116		# statement primitives do not return results (but will clobber r0/rax)
117		call _prints
118		# pop {rcx, rdx, rsi}
119		mov rcx, qword ptr [rsp]
120		mov rdx, qword ptr [rsp + 8]
121		mov rsi, qword ptr [rsp + 16]
122		add rsp, 24
123		mov rax, r15
124		mov rcx, rax
125		cmp rcx, 0
126		je _errNull
127		mov rax, qword ptr [rcx + 8]
128		mov rax, rax
129		mov rdx, rax
130		mov rax, 0
131		mov rsi, rax
132		jmp .L0
133	.L1:
134		cmp rcx, 0
135		je _errNull
136		mov rax, qword ptr [rcx]
137		mov rax, rax
138		push rax
139		pop rax
140		mov rax, rax
141		mov rsi, rax
142		# push {rcx, rdx, rsi}
143		sub rsp, 24
144		mov qword ptr [rsp], rcx
145		mov qword ptr [rsp + 8], rdx
146		mov qword ptr [rsp + 16], rsi
147		# Set up R11 as a temporary second base pointer for the caller saved things
148		mov r11, rsp
149		# Stack pointer unchanged, no stack allocated arguments
150		mov rax, rsi
151		mov rdi, rax
152		# statement primitives do not return results (but will clobber r0/rax)
153		call _printi
154		# pop {rcx, rdx, rsi}
155		mov rcx, qword ptr [rsp]
156		mov rdx, qword ptr [rsp + 8]
157		mov rsi, qword ptr [rsp + 16]
158		add rsp, 24
159		# push {rcx, rdx, rsi}
160		sub rsp, 24
161		mov qword ptr [rsp], rcx
162		mov qword ptr [rsp + 8], rdx
163		mov qword ptr [rsp + 16], rsi
164		# Set up R11 as a temporary second base pointer for the caller saved things
165		mov r11, rsp
166		# Stack pointer unchanged, no stack allocated arguments
167		lea rax, [rip + .L.str1]
168		push rax
169		pop rax
170		mov rax, rax
171		mov rdi, rax
172		# statement primitives do not return results (but will clobber r0/rax)
173		call _prints
174		# pop {rcx, rdx, rsi}
175		mov rcx, qword ptr [rsp]
176		mov rdx, qword ptr [rsp + 8]
177		mov rsi, qword ptr [rsp + 16]
178		add rsp, 24
179		mov rax, rdx
180		mov rcx, rax
181		cmp rcx, 0
182		je _errNull
183		mov rax, qword ptr [rcx + 8]
184		mov rax, rax
185		push rax
186		pop rax
187		mov rax, rax
188		mov rdx, rax
189	.L0:
190		cmp rdx, 0
191		jne .L1
192		cmp rcx, 0
193		je _errNull
194		mov rax, qword ptr [rcx]
195		mov rax, rax
196		push rax
197		pop rax
198		mov rax, rax
199		mov rsi, rax
200		# push {rcx, rdx, rsi}
201		sub rsp, 24
202		mov qword ptr [rsp], rcx
203		mov qword ptr [rsp + 8], rdx
204		mov qword ptr [rsp + 16], rsi
205		# Set up R11 as a temporary second base pointer for the caller saved things
206		mov r11, rsp
207		# Stack pointer unchanged, no stack allocated arguments
208		mov rax, rsi
209		mov rdi, rax
210		# statement primitives do not return results (but will clobber r0/rax)
211		call _printi
212		# pop {rcx, rdx, rsi}
213		mov rcx, qword ptr [rsp]
214		mov rdx, qword ptr [rsp + 8]
215		mov rsi, qword ptr [rsp + 16]
216		add rsp, 24
217		# push {rcx, rdx, rsi}
218		sub rsp, 24
219		mov qword ptr [rsp], rcx
220		mov qword ptr [rsp + 8], rdx
221		mov qword ptr [rsp + 16], rsi
222		# Set up R11 as a temporary second base pointer for the caller saved things
223		mov r11, rsp
224		# Stack pointer unchanged, no stack allocated arguments
225		lea rax, [rip + .L.str2]
226		push rax
227		pop rax
228		mov rax, rax
229		mov rdi, rax
230		# statement primitives do not return results (but will clobber r0/rax)
231		call _prints
232		call _println
233		# pop {rcx, rdx, rsi}
234		mov rcx, qword ptr [rsp]
235		mov rdx, qword ptr [rsp + 8]
236		mov rsi, qword ptr [rsp + 16]
237		add rsp, 24
238		# Stack pointer unchanged, no stack allocated variables
239		mov rax, 0
240		# pop {rbx, r12, r13, r14, r15}
241		mov rbx, qword ptr [rsp]
242		mov r12, qword ptr [rsp + 8]
243		mov r13, qword ptr [rsp + 16]
244		mov r14, qword ptr [rsp + 24]
245		mov r15, qword ptr [rsp + 32]
246		add rsp, 40
247		pop rbp
248		ret
249	
250	.section .rodata
251	# length of .L._prints_str0
252		.int 4
253	.L._prints_str0:
254		.asciz "%.*s"
255	.text
256	_prints:
257		push rbp
258		mov rbp, rsp
259		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
260		and rsp, -16
261		mov rdx, rdi
262		mov esi, dword ptr [rdi - 4]
263		lea rdi, [rip + .L._prints_str0]
264		# on x86, al represents the number of SIMD registers used as variadic arguments
265		mov al, 0
266		call printf@plt
267		mov rdi, 0
268		call fflush@plt
269		mov rsp, rbp
270		pop rbp
271		ret
272	
273	_malloc:
274		push rbp
275		mov rbp, rsp
276		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
277		and rsp, -16
278		call malloc@plt
279		cmp rax, 0
280		je _errOutOfMemory
281		mov rsp, rbp
282		pop rbp
283		ret
284	
285	.section .rodata
286	# length of .L._printi_str0
287		.int 2
288	.L._printi_str0:
289		.asciz "%d"
290	.text
291	_printi:
292		push rbp
293		mov rbp, rsp
294		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
295		and rsp, -16
296		mov esi, edi
297		lea rdi, [rip + .L._printi_str0]
298		# on x86, al represents the number of SIMD registers used as variadic arguments
299		mov al, 0
300		call printf@plt
301		mov rdi, 0
302		call fflush@plt
303		mov rsp, rbp
304		pop rbp
305		ret
306	
307	.section .rodata
308	# length of .L._println_str0
309		.int 0
310	.L._println_str0:
311		.asciz ""
312	.text
313	_println:
314		push rbp
315		mov rbp, rsp
316		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
317		and rsp, -16
318		lea rdi, [rip + .L._println_str0]
319		call puts@plt
320		mov rdi, 0
321		call fflush@plt
322		mov rsp, rbp
323		pop rbp
324		ret
325	
326	_exit:
327		push rbp
328		mov rbp, rsp
329		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
330		and rsp, -16
331		call exit@plt
332		mov rsp, rbp
333		pop rbp
334		ret
335	
336	.section .rodata
337	# length of .L._errOutOfMemory_str0
338		.int 27
339	.L._errOutOfMemory_str0:
340		.asciz "fatal error: out of memory\n"
341	.text
342	_errOutOfMemory:
343		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
344		and rsp, -16
345		lea rdi, [rip + .L._errOutOfMemory_str0]
346		call _prints
347		mov dil, -1
348		call exit@plt
349	
350	.section .rodata
351	# length of .L._errNull_str0
352		.int 45
353	.L._errNull_str0:
354		.asciz "fatal error: null pair dereferenced or freed\n"
355	.text
356	_errNull:
357		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
358		and rsp, -16
359		lea rdi, [rip + .L._errNull_str0]
360		call _prints
361		mov dil, -1
362		call exit@plt
===========================================================
-- Finished

