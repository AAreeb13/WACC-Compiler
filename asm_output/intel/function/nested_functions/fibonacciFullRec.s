./valid/function/nested_functions/fibonacciFullRec.wacc
calling the reference compiler on ./valid/function/nested_functions/fibonacciFullRec.wacc
-- Test: fibonacciFullRec.wacc

-- Uploaded file: 
---------------------------------------------------------------
# recursively calculate the nth fibonacci number

# Input: 30

# Output:
# This program calculates the nth fibonacci number recursively.
# Please enter n (should not be too large): The input n is 30
# The nth fibonacci number is 832040
#

# Program:

begin
  int fibonacci(int n) is
    if n <= 1
    then
      return n
    else
      skip
    fi ;
    int f1 = call fibonacci(n - 1) ;
    int f2 = call fibonacci(n - 2) ;
    return f1 + f2
  end

  println "This program calculates the nth fibonacci number recursively." ;
  print "Please enter n (should not be too large): " ;
  int n = 0;
  read n ;
  print "The input n is " ;
  println n ;
  print "The nth fibonacci number is " ;
  int result = call fibonacci(n) ;
  println  result
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciFullRec.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 61
5	.L.str0:
6		.asciz "This program calculates the nth fibonacci number recursively."
7	# length of .L.str1
8		.int 42
9	.L.str1:
10		.asciz "Please enter n (should not be too large): "
11	# length of .L.str2
12		.int 15
13	.L.str2:
14		.asciz "The input n is "
15	# length of .L.str3
16		.int 28
17	.L.str3:
18		.asciz "The nth fibonacci number is "
19	.text
20	main:
21		push rbp
22		# push {rbx, r12, r13}
23		sub rsp, 24
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov qword ptr [rsp + 16], r13
27		mov rbp, rsp
28		# Stack pointer unchanged, no stack allocated variables
29		# Stack pointer unchanged, no stack allocated arguments
30		lea rax, [rip + .L.str0]
31		push rax
32		pop rax
33		mov rax, rax
34		mov rdi, rax
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _prints
37		call _println
38		# Stack pointer unchanged, no stack allocated arguments
39		lea rax, [rip + .L.str1]
40		push rax
41		pop rax
42		mov rax, rax
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _prints
46		mov rax, 0
47		mov r12, rax
48		# Stack pointer unchanged, no stack allocated arguments
49		# load the current value in the destination of the read so it supports defaults
50		mov rax, r12
51		mov rdi, rax
52		call _readi
53		mov r11, rax
54		mov rax, r11
55		mov r12, rax
56		# Stack pointer unchanged, no stack allocated arguments
57		lea rax, [rip + .L.str2]
58		push rax
59		pop rax
60		mov rax, rax
61		mov rdi, rax
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _prints
64		# Stack pointer unchanged, no stack allocated arguments
65		mov rax, r12
66		mov rdi, rax
67		# statement primitives do not return results (but will clobber r0/rax)
68		call _printi
69		call _println
70		# Stack pointer unchanged, no stack allocated arguments
71		lea rax, [rip + .L.str3]
72		push rax
73		pop rax
74		mov rax, rax
75		mov rdi, rax
76		# statement primitives do not return results (but will clobber r0/rax)
77		call _prints
78		# Stack pointer unchanged, no stack allocated arguments
79		mov rax, r12
80		mov rdi, rax
81		call wacc_fibonacci
82		mov r11, rax
83		# Stack pointer unchanged, no stack allocated arguments
84		mov rax, r11
85		mov r13, rax
86		# Stack pointer unchanged, no stack allocated arguments
87		mov rax, r13
88		mov rdi, rax
89		# statement primitives do not return results (but will clobber r0/rax)
90		call _printi
91		call _println
92		# Stack pointer unchanged, no stack allocated variables
93		mov rax, 0
94		# pop {rbx, r12, r13}
95		mov rbx, qword ptr [rsp]
96		mov r12, qword ptr [rsp + 8]
97		mov r13, qword ptr [rsp + 16]
98		add rsp, 24
99		pop rbp
100		ret
101	
102	wacc_fibonacci:
103		push rbp
104		# push {r12, r13}
105		sub rsp, 16
106		mov qword ptr [rsp], r12
107		mov qword ptr [rsp + 8], r13
108		mov rbp, rsp
109		# Stack pointer unchanged, no stack allocated variables
110		cmp rdi, 1
111		jle .L0
112		jmp .L1
113	.L0:
114		mov rax, rdi
115		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
116		mov rsp, rbp
117		# pop {r12, r13}
118		mov r12, qword ptr [rsp]
119		mov r13, qword ptr [rsp + 8]
120		add rsp, 16
121		pop rbp
122		ret
123	.L1:
124		push rdi
125		# Set up R11 as a temporary second base pointer for the caller saved things
126		mov r11, rsp
127		# Stack pointer unchanged, no stack allocated arguments
128		mov eax, edi
129		sub eax, 1
130		jo _errOverflow
131		movsx rax, eax
132		push rax
133		pop rax
134		mov rax, rax
135		mov rdi, rax
136		call wacc_fibonacci
137		mov r11, rax
138		# Stack pointer unchanged, no stack allocated arguments
139		pop rdi
140		mov rax, r11
141		mov r12, rax
142		push rdi
143		# Set up R11 as a temporary second base pointer for the caller saved things
144		mov r11, rsp
145		# Stack pointer unchanged, no stack allocated arguments
146		mov eax, edi
147		sub eax, 2
148		jo _errOverflow
149		movsx rax, eax
150		push rax
151		pop rax
152		mov rax, rax
153		mov rdi, rax
154		call wacc_fibonacci
155		mov r11, rax
156		# Stack pointer unchanged, no stack allocated arguments
157		pop rdi
158		mov rax, r11
159		mov r13, rax
160		mov eax, r12d
161		add eax, r13d
162		jo _errOverflow
163		movsx rax, eax
164		push rax
165		pop rax
166		mov rax, rax
167		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
168		mov rsp, rbp
169		# pop {r12, r13}
170		mov r12, qword ptr [rsp]
171		mov r13, qword ptr [rsp + 8]
172		add rsp, 16
173		pop rbp
174		ret
175		# Stack pointer unchanged, no stack allocated variables
176		# 'ere be dragons: this is 100% dead code, functions always end in returns!
177	
178	.section .rodata
179	# length of .L._prints_str0
180		.int 4
181	.L._prints_str0:
182		.asciz "%.*s"
183	.text
184	_prints:
185		push rbp
186		mov rbp, rsp
187		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
188		and rsp, -16
189		mov rdx, rdi
190		mov esi, dword ptr [rdi - 4]
191		lea rdi, [rip + .L._prints_str0]
192		# on x86, al represents the number of SIMD registers used as variadic arguments
193		mov al, 0
194		call printf@plt
195		mov rdi, 0
196		call fflush@plt
197		mov rsp, rbp
198		pop rbp
199		ret
200	
201	.section .rodata
202	# length of .L._readi_str0
203		.int 2
204	.L._readi_str0:
205		.asciz "%d"
206	.text
207	_readi:
208		push rbp
209		mov rbp, rsp
210		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
211		and rsp, -16
212		# RDI contains the "original" value of the destination of the read
213		# allocate space on the stack to store the read: preserve alignment!
214		# the passed default argument should be stored in case of EOF
215		sub rsp, 16
216		mov dword ptr [rsp], edi
217		lea rsi, qword ptr [rsp]
218		lea rdi, [rip + .L._readi_str0]
219		# on x86, al represents the number of SIMD registers used as variadic arguments
220		mov al, 0
221		call scanf@plt
222		movsx rax, dword ptr [rsp]
223		add rsp, 16
224		mov rsp, rbp
225		pop rbp
226		ret
227	
228	.section .rodata
229	# length of .L._printi_str0
230		.int 2
231	.L._printi_str0:
232		.asciz "%d"
233	.text
234	_printi:
235		push rbp
236		mov rbp, rsp
237		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
238		and rsp, -16
239		mov esi, edi
240		lea rdi, [rip + .L._printi_str0]
241		# on x86, al represents the number of SIMD registers used as variadic arguments
242		mov al, 0
243		call printf@plt
244		mov rdi, 0
245		call fflush@plt
246		mov rsp, rbp
247		pop rbp
248		ret
249	
250	.section .rodata
251	# length of .L._println_str0
252		.int 0
253	.L._println_str0:
254		.asciz ""
255	.text
256	_println:
257		push rbp
258		mov rbp, rsp
259		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
260		and rsp, -16
261		lea rdi, [rip + .L._println_str0]
262		call puts@plt
263		mov rdi, 0
264		call fflush@plt
265		mov rsp, rbp
266		pop rbp
267		ret
268	
269	_exit:
270		push rbp
271		mov rbp, rsp
272		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
273		and rsp, -16
274		call exit@plt
275		mov rsp, rbp
276		pop rbp
277		ret
278	
279	.section .rodata
280	# length of .L._errOverflow_str0
281		.int 52
282	.L._errOverflow_str0:
283		.asciz "fatal error: integer overflow or underflow occurred\n"
284	.text
285	_errOverflow:
286		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
287		and rsp, -16
288		lea rdi, [rip + .L._errOverflow_str0]
289		call _prints
290		mov dil, -1
291		call exit@plt
===========================================================
-- Finished

