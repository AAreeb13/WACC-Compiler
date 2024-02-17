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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 2
4	.L.str0:
5		.asciz ", "
6	# length of .L.str1
7		.int 35
8	.L.str1:
9		.asciz "The first 20 fibonacci numbers are:"
10	# length of .L.str2
11		.int 3
12	.L.str2:
13		.asciz "0, "
14	# length of .L.str3
15		.int 3
16	.L.str3:
17		.asciz "..."
18	.text
19	main:
20		pushq %rbp
21		# pushq {%rbx, %r12}
22		subq $16, %rsp
23		movq %rbx, (%rsp)
24		movq %r12, 8(%rsp)
25		movq %rsp, %rbp
26		# Stack pointer unchanged, no stack allocated variables
27		# Stack pointer unchanged, no stack allocated arguments
28		leaq .L.str1(%rip), %rax
29		pushq %rax
30		popq %rax
31		movq %rax, %rax
32		movq %rax, %rdi
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _prints
35		call _println
36		# Stack pointer unchanged, no stack allocated arguments
37		leaq .L.str2(%rip), %rax
38		pushq %rax
39		popq %rax
40		movq %rax, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _prints
44		# Stack pointer unchanged, no stack allocated arguments
45		movq $19, %rax
46		movq %rax, %rdi
47		movq $1, %rax
48		movq %rax, %rsi
49		call wacc_fibonacci
50		movq %rax, %r11
51		# Stack pointer unchanged, no stack allocated arguments
52		movq %r11, %rax
53		movq %rax, %r12
54		# Stack pointer unchanged, no stack allocated arguments
55		movq %r12, %rax
56		movq %rax, %rdi
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _printi
59		# Stack pointer unchanged, no stack allocated arguments
60		leaq .L.str3(%rip), %rax
61		pushq %rax
62		popq %rax
63		movq %rax, %rax
64		movq %rax, %rdi
65		# statement primitives do not return results (but will clobber r0/rax)
66		call _prints
67		call _println
68		# Stack pointer unchanged, no stack allocated variables
69		movq $0, %rax
70		# popq {%rbx, %r12}
71		movq (%rsp), %rbx
72		movq 8(%rsp), %r12
73		addq $16, %rsp
74		popq %rbp
75		ret
76	
77	wacc_fibonacci:
78		pushq %rbp
79		# pushq {%r12, %r13}
80		subq $16, %rsp
81		movq %r12, (%rsp)
82		movq %r13, 8(%rsp)
83		movq %rsp, %rbp
84		# Stack pointer unchanged, no stack allocated variables
85		cmpq $1, %rdi
86		jle .L0
87		jmp .L1
88	.L0:
89		movq %rdi, %rax
90		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
91		movq %rbp, %rsp
92		# popq {%r12, %r13}
93		movq (%rsp), %r12
94		movq 8(%rsp), %r13
95		addq $16, %rsp
96		popq %rbp
97		ret
98	.L1:
99		# pushq {%rsi, %rdi}
100		subq $16, %rsp
101		movq %rsi, (%rsp)
102		movq %rdi, 8(%rsp)
103		# Set up R11 as a temporary second base pointer for the caller saved things
104		movq %rsp, %r11
105		# Stack pointer unchanged, no stack allocated arguments
106		movl %edi, %eax
107		subl $1, %eax
108		jo _errOverflow
109		movslq %eax, %rax
110		pushq %rax
111		popq %rax
112		movq %rax, %rax
113		movq %rax, %rdi
114		movq %rsi, %rax
115		movq %rax, %rsi
116		call wacc_fibonacci
117		movq %rax, %r11
118		# Stack pointer unchanged, no stack allocated arguments
119		# popq {%rsi, %rdi}
120		movq (%rsp), %rsi
121		movq 8(%rsp), %rdi
122		addq $16, %rsp
123		movq %r11, %rax
124		movq %rax, %r12
125		cmpq $1, %rsi
126		je .L2
127		jmp .L3
128	.L2:
129		# pushq {%rsi, %rdi}
130		subq $16, %rsp
131		movq %rsi, (%rsp)
132		movq %rdi, 8(%rsp)
133		# Set up R11 as a temporary second base pointer for the caller saved things
134		movq %rsp, %r11
135		# Stack pointer unchanged, no stack allocated arguments
136		movq %r12, %rax
137		movq %rax, %rdi
138		# statement primitives do not return results (but will clobber r0/rax)
139		call _printi
140		# popq {%rsi, %rdi}
141		movq (%rsp), %rsi
142		movq 8(%rsp), %rdi
143		addq $16, %rsp
144		# pushq {%rsi, %rdi}
145		subq $16, %rsp
146		movq %rsi, (%rsp)
147		movq %rdi, 8(%rsp)
148		# Set up R11 as a temporary second base pointer for the caller saved things
149		movq %rsp, %r11
150		# Stack pointer unchanged, no stack allocated arguments
151		leaq .L.str0(%rip), %rax
152		pushq %rax
153		popq %rax
154		movq %rax, %rax
155		movq %rax, %rdi
156		# statement primitives do not return results (but will clobber r0/rax)
157		call _prints
158		# popq {%rsi, %rdi}
159		movq (%rsp), %rsi
160		movq 8(%rsp), %rdi
161		addq $16, %rsp
162	.L3:
163		# pushq {%rsi, %rdi}
164		subq $16, %rsp
165		movq %rsi, (%rsp)
166		movq %rdi, 8(%rsp)
167		# Set up R11 as a temporary second base pointer for the caller saved things
168		movq %rsp, %r11
169		# Stack pointer unchanged, no stack allocated arguments
170		movl %edi, %eax
171		subl $2, %eax
172		jo _errOverflow
173		movslq %eax, %rax
174		pushq %rax
175		popq %rax
176		movq %rax, %rax
177		movq %rax, %rdi
178		movq $0, %rax
179		movq %rax, %rsi
180		call wacc_fibonacci
181		movq %rax, %r11
182		# Stack pointer unchanged, no stack allocated arguments
183		# popq {%rsi, %rdi}
184		movq (%rsp), %rsi
185		movq 8(%rsp), %rdi
186		addq $16, %rsp
187		movq %r11, %rax
188		movq %rax, %r13
189		movl %r12d, %eax
190		addl %r13d, %eax
191		jo _errOverflow
192		movslq %eax, %rax
193		pushq %rax
194		popq %rax
195		movq %rax, %rax
196		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
197		movq %rbp, %rsp
198		# popq {%r12, %r13}
199		movq (%rsp), %r12
200		movq 8(%rsp), %r13
201		addq $16, %rsp
202		popq %rbp
203		ret
204		# Stack pointer unchanged, no stack allocated variables
205		# 'ere be dragons: this is 100% dead code, functions always end in returns!
206	
207	.section .rodata
208	# length of .L._prints_str0
209		.int 4
210	.L._prints_str0:
211		.asciz "%.*s"
212	.text
213	_prints:
214		pushq %rbp
215		movq %rsp, %rbp
216		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
217		andq $-16, %rsp
218		movq %rdi, %rdx
219		movl -4(%rdi), %esi
220		leaq .L._prints_str0(%rip), %rdi
221		# on x86, al represents the number of SIMD registers used as variadic arguments
222		movb $0, %al
223		call printf@plt
224		movq $0, %rdi
225		call fflush@plt
226		movq %rbp, %rsp
227		popq %rbp
228		ret
229	
230	.section .rodata
231	# length of .L._printi_str0
232		.int 2
233	.L._printi_str0:
234		.asciz "%d"
235	.text
236	_printi:
237		pushq %rbp
238		movq %rsp, %rbp
239		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
240		andq $-16, %rsp
241		movl %edi, %esi
242		leaq .L._printi_str0(%rip), %rdi
243		# on x86, al represents the number of SIMD registers used as variadic arguments
244		movb $0, %al
245		call printf@plt
246		movq $0, %rdi
247		call fflush@plt
248		movq %rbp, %rsp
249		popq %rbp
250		ret
251	
252	.section .rodata
253	# length of .L._println_str0
254		.int 0
255	.L._println_str0:
256		.asciz ""
257	.text
258	_println:
259		pushq %rbp
260		movq %rsp, %rbp
261		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
262		andq $-16, %rsp
263		leaq .L._println_str0(%rip), %rdi
264		call puts@plt
265		movq $0, %rdi
266		call fflush@plt
267		movq %rbp, %rsp
268		popq %rbp
269		ret
270	
271	_exit:
272		pushq %rbp
273		movq %rsp, %rbp
274		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
275		andq $-16, %rsp
276		call exit@plt
277		movq %rbp, %rsp
278		popq %rbp
279		ret
280	
281	.section .rodata
282	# length of .L._errOverflow_str0
283		.int 52
284	.L._errOverflow_str0:
285		.asciz "fatal error: integer overflow or underflow occurred\n"
286	.text
287	_errOverflow:
288		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
289		andq $-16, %rsp
290		leaq .L._errOverflow_str0(%rip), %rdi
291		call _prints
292		movb $-1, %dil
293		call exit@plt
===========================================================
-- Finished

