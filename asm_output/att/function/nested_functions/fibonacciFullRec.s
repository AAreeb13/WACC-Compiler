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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 61
4	.L.str0:
5		.asciz "This program calculates the nth fibonacci number recursively."
6	# length of .L.str1
7		.int 42
8	.L.str1:
9		.asciz "Please enter n (should not be too large): "
10	# length of .L.str2
11		.int 15
12	.L.str2:
13		.asciz "The input n is "
14	# length of .L.str3
15		.int 28
16	.L.str3:
17		.asciz "The nth fibonacci number is "
18	.text
19	main:
20		pushq %rbp
21		# pushq {%rbx, %r12, %r13}
22		subq $24, %rsp
23		movq %rbx, (%rsp)
24		movq %r12, 8(%rsp)
25		movq %r13, 16(%rsp)
26		movq %rsp, %rbp
27		# Stack pointer unchanged, no stack allocated variables
28		# Stack pointer unchanged, no stack allocated arguments
29		leaq .L.str0(%rip), %rax
30		pushq %rax
31		popq %rax
32		movq %rax, %rax
33		movq %rax, %rdi
34		# statement primitives do not return results (but will clobber r0/rax)
35		call _prints
36		call _println
37		# Stack pointer unchanged, no stack allocated arguments
38		leaq .L.str1(%rip), %rax
39		pushq %rax
40		popq %rax
41		movq %rax, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		movq $0, %rax
46		movq %rax, %r12
47		# Stack pointer unchanged, no stack allocated arguments
48		# load the current value in the destination of the read so it supports defaults
49		movq %r12, %rax
50		movq %rax, %rdi
51		call _readi
52		movq %rax, %r11
53		movq %r11, %rax
54		movq %rax, %r12
55		# Stack pointer unchanged, no stack allocated arguments
56		leaq .L.str2(%rip), %rax
57		pushq %rax
58		popq %rax
59		movq %rax, %rax
60		movq %rax, %rdi
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _prints
63		# Stack pointer unchanged, no stack allocated arguments
64		movq %r12, %rax
65		movq %rax, %rdi
66		# statement primitives do not return results (but will clobber r0/rax)
67		call _printi
68		call _println
69		# Stack pointer unchanged, no stack allocated arguments
70		leaq .L.str3(%rip), %rax
71		pushq %rax
72		popq %rax
73		movq %rax, %rax
74		movq %rax, %rdi
75		# statement primitives do not return results (but will clobber r0/rax)
76		call _prints
77		# Stack pointer unchanged, no stack allocated arguments
78		movq %r12, %rax
79		movq %rax, %rdi
80		call wacc_fibonacci
81		movq %rax, %r11
82		# Stack pointer unchanged, no stack allocated arguments
83		movq %r11, %rax
84		movq %rax, %r13
85		# Stack pointer unchanged, no stack allocated arguments
86		movq %r13, %rax
87		movq %rax, %rdi
88		# statement primitives do not return results (but will clobber r0/rax)
89		call _printi
90		call _println
91		# Stack pointer unchanged, no stack allocated variables
92		movq $0, %rax
93		# popq {%rbx, %r12, %r13}
94		movq (%rsp), %rbx
95		movq 8(%rsp), %r12
96		movq 16(%rsp), %r13
97		addq $24, %rsp
98		popq %rbp
99		ret
100	
101	wacc_fibonacci:
102		pushq %rbp
103		# pushq {%r12, %r13}
104		subq $16, %rsp
105		movq %r12, (%rsp)
106		movq %r13, 8(%rsp)
107		movq %rsp, %rbp
108		# Stack pointer unchanged, no stack allocated variables
109		cmpq $1, %rdi
110		jle .L0
111		jmp .L1
112	.L0:
113		movq %rdi, %rax
114		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
115		movq %rbp, %rsp
116		# popq {%r12, %r13}
117		movq (%rsp), %r12
118		movq 8(%rsp), %r13
119		addq $16, %rsp
120		popq %rbp
121		ret
122	.L1:
123		pushq %rdi
124		# Set up R11 as a temporary second base pointer for the caller saved things
125		movq %rsp, %r11
126		# Stack pointer unchanged, no stack allocated arguments
127		movl %edi, %eax
128		subl $1, %eax
129		jo _errOverflow
130		movslq %eax, %rax
131		pushq %rax
132		popq %rax
133		movq %rax, %rax
134		movq %rax, %rdi
135		call wacc_fibonacci
136		movq %rax, %r11
137		# Stack pointer unchanged, no stack allocated arguments
138		popq %rdi
139		movq %r11, %rax
140		movq %rax, %r12
141		pushq %rdi
142		# Set up R11 as a temporary second base pointer for the caller saved things
143		movq %rsp, %r11
144		# Stack pointer unchanged, no stack allocated arguments
145		movl %edi, %eax
146		subl $2, %eax
147		jo _errOverflow
148		movslq %eax, %rax
149		pushq %rax
150		popq %rax
151		movq %rax, %rax
152		movq %rax, %rdi
153		call wacc_fibonacci
154		movq %rax, %r11
155		# Stack pointer unchanged, no stack allocated arguments
156		popq %rdi
157		movq %r11, %rax
158		movq %rax, %r13
159		movl %r12d, %eax
160		addl %r13d, %eax
161		jo _errOverflow
162		movslq %eax, %rax
163		pushq %rax
164		popq %rax
165		movq %rax, %rax
166		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
167		movq %rbp, %rsp
168		# popq {%r12, %r13}
169		movq (%rsp), %r12
170		movq 8(%rsp), %r13
171		addq $16, %rsp
172		popq %rbp
173		ret
174		# Stack pointer unchanged, no stack allocated variables
175		# 'ere be dragons: this is 100% dead code, functions always end in returns!
176	
177	.section .rodata
178	# length of .L._prints_str0
179		.int 4
180	.L._prints_str0:
181		.asciz "%.*s"
182	.text
183	_prints:
184		pushq %rbp
185		movq %rsp, %rbp
186		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
187		andq $-16, %rsp
188		movq %rdi, %rdx
189		movl -4(%rdi), %esi
190		leaq .L._prints_str0(%rip), %rdi
191		# on x86, al represents the number of SIMD registers used as variadic arguments
192		movb $0, %al
193		call printf@plt
194		movq $0, %rdi
195		call fflush@plt
196		movq %rbp, %rsp
197		popq %rbp
198		ret
199	
200	.section .rodata
201	# length of .L._readi_str0
202		.int 2
203	.L._readi_str0:
204		.asciz "%d"
205	.text
206	_readi:
207		pushq %rbp
208		movq %rsp, %rbp
209		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
210		andq $-16, %rsp
211		# RDI contains the "original" value of the destination of the read
212		# allocate space on the stack to store the read: preserve alignment!
213		# the passed default argument should be stored in case of EOF
214		subq $16, %rsp
215		movl %edi, (%rsp)
216		leaq (%rsp), %rsi
217		leaq .L._readi_str0(%rip), %rdi
218		# on x86, al represents the number of SIMD registers used as variadic arguments
219		movb $0, %al
220		call scanf@plt
221		movslq (%rsp), %rax
222		addq $16, %rsp
223		movq %rbp, %rsp
224		popq %rbp
225		ret
226	
227	.section .rodata
228	# length of .L._printi_str0
229		.int 2
230	.L._printi_str0:
231		.asciz "%d"
232	.text
233	_printi:
234		pushq %rbp
235		movq %rsp, %rbp
236		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
237		andq $-16, %rsp
238		movl %edi, %esi
239		leaq .L._printi_str0(%rip), %rdi
240		# on x86, al represents the number of SIMD registers used as variadic arguments
241		movb $0, %al
242		call printf@plt
243		movq $0, %rdi
244		call fflush@plt
245		movq %rbp, %rsp
246		popq %rbp
247		ret
248	
249	.section .rodata
250	# length of .L._println_str0
251		.int 0
252	.L._println_str0:
253		.asciz ""
254	.text
255	_println:
256		pushq %rbp
257		movq %rsp, %rbp
258		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
259		andq $-16, %rsp
260		leaq .L._println_str0(%rip), %rdi
261		call puts@plt
262		movq $0, %rdi
263		call fflush@plt
264		movq %rbp, %rsp
265		popq %rbp
266		ret
267	
268	_exit:
269		pushq %rbp
270		movq %rsp, %rbp
271		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
272		andq $-16, %rsp
273		call exit@plt
274		movq %rbp, %rsp
275		popq %rbp
276		ret
277	
278	.section .rodata
279	# length of .L._errOverflow_str0
280		.int 52
281	.L._errOverflow_str0:
282		.asciz "fatal error: integer overflow or underflow occurred\n"
283	.text
284	_errOverflow:
285		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
286		andq $-16, %rsp
287		leaq .L._errOverflow_str0(%rip), %rdi
288		call _prints
289		movb $-1, %dil
290		call exit@plt
===========================================================
-- Finished

