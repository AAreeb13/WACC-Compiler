./valid/expressions/sequentialCount.wacc
calling the reference compiler on ./valid/expressions/sequentialCount.wacc
-- Test: sequentialCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple sequential counting

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sequentialCount.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 20
4	.L.str0:
5		.asciz "Can you count to 10?"
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		movq $1, %rax
16		movq %rax, %r12
17		# Stack pointer unchanged, no stack allocated arguments
18		leaq .L.str0(%rip), %rax
19		pushq %rax
20		popq %rax
21		movq %rax, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _prints
25		call _println
26		# Stack pointer unchanged, no stack allocated arguments
27		movq %r12, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _printi
31		call _println
32		movl %r12d, %eax
33		addl $1, %eax
34		jo _errOverflow
35		movslq %eax, %rax
36		pushq %rax
37		popq %rax
38		movq %rax, %rax
39		movq %rax, %r12
40		# Stack pointer unchanged, no stack allocated arguments
41		movq %r12, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printi
45		call _println
46		movl %r12d, %eax
47		addl $1, %eax
48		jo _errOverflow
49		movslq %eax, %rax
50		pushq %rax
51		popq %rax
52		movq %rax, %rax
53		movq %rax, %r12
54		# Stack pointer unchanged, no stack allocated arguments
55		movq %r12, %rax
56		movq %rax, %rdi
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _printi
59		call _println
60		movl %r12d, %eax
61		addl $1, %eax
62		jo _errOverflow
63		movslq %eax, %rax
64		pushq %rax
65		popq %rax
66		movq %rax, %rax
67		movq %rax, %r12
68		# Stack pointer unchanged, no stack allocated arguments
69		movq %r12, %rax
70		movq %rax, %rdi
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _printi
73		call _println
74		movl %r12d, %eax
75		addl $1, %eax
76		jo _errOverflow
77		movslq %eax, %rax
78		pushq %rax
79		popq %rax
80		movq %rax, %rax
81		movq %rax, %r12
82		# Stack pointer unchanged, no stack allocated arguments
83		movq %r12, %rax
84		movq %rax, %rdi
85		# statement primitives do not return results (but will clobber r0/rax)
86		call _printi
87		call _println
88		movl %r12d, %eax
89		addl $1, %eax
90		jo _errOverflow
91		movslq %eax, %rax
92		pushq %rax
93		popq %rax
94		movq %rax, %rax
95		movq %rax, %r12
96		# Stack pointer unchanged, no stack allocated arguments
97		movq %r12, %rax
98		movq %rax, %rdi
99		# statement primitives do not return results (but will clobber r0/rax)
100		call _printi
101		call _println
102		movl %r12d, %eax
103		addl $1, %eax
104		jo _errOverflow
105		movslq %eax, %rax
106		pushq %rax
107		popq %rax
108		movq %rax, %rax
109		movq %rax, %r12
110		# Stack pointer unchanged, no stack allocated arguments
111		movq %r12, %rax
112		movq %rax, %rdi
113		# statement primitives do not return results (but will clobber r0/rax)
114		call _printi
115		call _println
116		movl %r12d, %eax
117		addl $1, %eax
118		jo _errOverflow
119		movslq %eax, %rax
120		pushq %rax
121		popq %rax
122		movq %rax, %rax
123		movq %rax, %r12
124		# Stack pointer unchanged, no stack allocated arguments
125		movq %r12, %rax
126		movq %rax, %rdi
127		# statement primitives do not return results (but will clobber r0/rax)
128		call _printi
129		call _println
130		movl %r12d, %eax
131		addl $1, %eax
132		jo _errOverflow
133		movslq %eax, %rax
134		pushq %rax
135		popq %rax
136		movq %rax, %rax
137		movq %rax, %r12
138		# Stack pointer unchanged, no stack allocated arguments
139		movq %r12, %rax
140		movq %rax, %rdi
141		# statement primitives do not return results (but will clobber r0/rax)
142		call _printi
143		call _println
144		movl %r12d, %eax
145		addl $1, %eax
146		jo _errOverflow
147		movslq %eax, %rax
148		pushq %rax
149		popq %rax
150		movq %rax, %rax
151		movq %rax, %r12
152		# Stack pointer unchanged, no stack allocated arguments
153		movq %r12, %rax
154		movq %rax, %rdi
155		# statement primitives do not return results (but will clobber r0/rax)
156		call _printi
157		call _println
158		# Stack pointer unchanged, no stack allocated variables
159		movq $0, %rax
160		# popq {%rbx, %r12}
161		movq (%rsp), %rbx
162		movq 8(%rsp), %r12
163		addq $16, %rsp
164		popq %rbp
165		ret
166	
167	.section .rodata
168	# length of .L._prints_str0
169		.int 4
170	.L._prints_str0:
171		.asciz "%.*s"
172	.text
173	_prints:
174		pushq %rbp
175		movq %rsp, %rbp
176		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
177		andq $-16, %rsp
178		movq %rdi, %rdx
179		movl -4(%rdi), %esi
180		leaq .L._prints_str0(%rip), %rdi
181		# on x86, al represents the number of SIMD registers used as variadic arguments
182		movb $0, %al
183		call printf@plt
184		movq $0, %rdi
185		call fflush@plt
186		movq %rbp, %rsp
187		popq %rbp
188		ret
189	
190	.section .rodata
191	# length of .L._printi_str0
192		.int 2
193	.L._printi_str0:
194		.asciz "%d"
195	.text
196	_printi:
197		pushq %rbp
198		movq %rsp, %rbp
199		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
200		andq $-16, %rsp
201		movl %edi, %esi
202		leaq .L._printi_str0(%rip), %rdi
203		# on x86, al represents the number of SIMD registers used as variadic arguments
204		movb $0, %al
205		call printf@plt
206		movq $0, %rdi
207		call fflush@plt
208		movq %rbp, %rsp
209		popq %rbp
210		ret
211	
212	.section .rodata
213	# length of .L._println_str0
214		.int 0
215	.L._println_str0:
216		.asciz ""
217	.text
218	_println:
219		pushq %rbp
220		movq %rsp, %rbp
221		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
222		andq $-16, %rsp
223		leaq .L._println_str0(%rip), %rdi
224		call puts@plt
225		movq $0, %rdi
226		call fflush@plt
227		movq %rbp, %rsp
228		popq %rbp
229		ret
230	
231	_exit:
232		pushq %rbp
233		movq %rsp, %rbp
234		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
235		andq $-16, %rsp
236		call exit@plt
237		movq %rbp, %rsp
238		popq %rbp
239		ret
240	
241	.section .rodata
242	# length of .L._errOverflow_str0
243		.int 52
244	.L._errOverflow_str0:
245		.asciz "fatal error: integer overflow or underflow occurred\n"
246	.text
247	_errOverflow:
248		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
249		andq $-16, %rsp
250		leaq .L._errOverflow_str0(%rip), %rdi
251		call _prints
252		movb $-1, %dil
253		call exit@plt
===========================================================
-- Finished

