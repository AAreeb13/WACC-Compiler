./valid/while/fibonacciFullIt.wacc
calling the reference compiler on ./valid/while/fibonacciFullIt.wacc
-- Test: fibonacciFullIt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iteratively calculate the given fibonacci number

# Input: 30

# Output:
# This program calculates the nth fibonacci number iteratively.
# Please enter n (should not be too large): The input n is 30
# The nth fibonacci number is 832040
#

# Program:

begin
  println "This program calculates the nth fibonacci number iteratively." ;
  print "Please enter n (should not be too large): " ;
  int n = 0;
  read n ;
  print "The input n is " ;
  println n ;
  print "The nth fibonacci number is " ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  while n > 0 do
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    n = n - 1
  done ;
  println f0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciFullIt.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 61
4	.L.str0:
5		.asciz "This program calculates the nth fibonacci number iteratively."
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
21		# pushq {%rbx, %r12, %r13, %r14, %r15}
22		subq $40, %rsp
23		movq %rbx, (%rsp)
24		movq %r12, 8(%rsp)
25		movq %r13, 16(%rsp)
26		movq %r14, 24(%rsp)
27		movq %r15, 32(%rsp)
28		movq %rsp, %rbp
29		# Stack pointer unchanged, no stack allocated variables
30		# Stack pointer unchanged, no stack allocated arguments
31		leaq .L.str0(%rip), %rax
32		pushq %rax
33		popq %rax
34		movq %rax, %rax
35		movq %rax, %rdi
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		call _println
39		# Stack pointer unchanged, no stack allocated arguments
40		leaq .L.str1(%rip), %rax
41		pushq %rax
42		popq %rax
43		movq %rax, %rax
44		movq %rax, %rdi
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _prints
47		movq $0, %rax
48		movq %rax, %r12
49		# Stack pointer unchanged, no stack allocated arguments
50		# load the current value in the destination of the read so it supports defaults
51		movq %r12, %rax
52		movq %rax, %rdi
53		call _readi
54		movq %rax, %r11
55		movq %r11, %rax
56		movq %rax, %r12
57		# Stack pointer unchanged, no stack allocated arguments
58		leaq .L.str2(%rip), %rax
59		pushq %rax
60		popq %rax
61		movq %rax, %rax
62		movq %rax, %rdi
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _prints
65		# Stack pointer unchanged, no stack allocated arguments
66		movq %r12, %rax
67		movq %rax, %rdi
68		# statement primitives do not return results (but will clobber r0/rax)
69		call _printi
70		call _println
71		# Stack pointer unchanged, no stack allocated arguments
72		leaq .L.str3(%rip), %rax
73		pushq %rax
74		popq %rax
75		movq %rax, %rax
76		movq %rax, %rdi
77		# statement primitives do not return results (but will clobber r0/rax)
78		call _prints
79		movq $0, %rax
80		movq %rax, %r13
81		movq $1, %rax
82		movq %rax, %r14
83		movq $0, %rax
84		movq %rax, %r15
85		jmp .L0
86	.L1:
87		movq %r13, %rax
88		movq %rax, %r15
89		movq %r14, %rax
90		movq %rax, %r13
91		movl %r15d, %eax
92		addl %r14d, %eax
93		jo _errOverflow
94		movslq %eax, %rax
95		pushq %rax
96		popq %rax
97		movq %rax, %rax
98		movq %rax, %r14
99		movl %r12d, %eax
100		subl $1, %eax
101		jo _errOverflow
102		movslq %eax, %rax
103		pushq %rax
104		popq %rax
105		movq %rax, %rax
106		movq %rax, %r12
107	.L0:
108		cmpq $0, %r12
109		jg .L1
110		# Stack pointer unchanged, no stack allocated arguments
111		movq %r13, %rax
112		movq %rax, %rdi
113		# statement primitives do not return results (but will clobber r0/rax)
114		call _printi
115		call _println
116		# Stack pointer unchanged, no stack allocated variables
117		movq $0, %rax
118		# popq {%rbx, %r12, %r13, %r14, %r15}
119		movq (%rsp), %rbx
120		movq 8(%rsp), %r12
121		movq 16(%rsp), %r13
122		movq 24(%rsp), %r14
123		movq 32(%rsp), %r15
124		addq $40, %rsp
125		popq %rbp
126		ret
127	
128	.section .rodata
129	# length of .L._prints_str0
130		.int 4
131	.L._prints_str0:
132		.asciz "%.*s"
133	.text
134	_prints:
135		pushq %rbp
136		movq %rsp, %rbp
137		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
138		andq $-16, %rsp
139		movq %rdi, %rdx
140		movl -4(%rdi), %esi
141		leaq .L._prints_str0(%rip), %rdi
142		# on x86, al represents the number of SIMD registers used as variadic arguments
143		movb $0, %al
144		call printf@plt
145		movq $0, %rdi
146		call fflush@plt
147		movq %rbp, %rsp
148		popq %rbp
149		ret
150	
151	.section .rodata
152	# length of .L._readi_str0
153		.int 2
154	.L._readi_str0:
155		.asciz "%d"
156	.text
157	_readi:
158		pushq %rbp
159		movq %rsp, %rbp
160		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
161		andq $-16, %rsp
162		# RDI contains the "original" value of the destination of the read
163		# allocate space on the stack to store the read: preserve alignment!
164		# the passed default argument should be stored in case of EOF
165		subq $16, %rsp
166		movl %edi, (%rsp)
167		leaq (%rsp), %rsi
168		leaq .L._readi_str0(%rip), %rdi
169		# on x86, al represents the number of SIMD registers used as variadic arguments
170		movb $0, %al
171		call scanf@plt
172		movslq (%rsp), %rax
173		addq $16, %rsp
174		movq %rbp, %rsp
175		popq %rbp
176		ret
177	
178	.section .rodata
179	# length of .L._printi_str0
180		.int 2
181	.L._printi_str0:
182		.asciz "%d"
183	.text
184	_printi:
185		pushq %rbp
186		movq %rsp, %rbp
187		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
188		andq $-16, %rsp
189		movl %edi, %esi
190		leaq .L._printi_str0(%rip), %rdi
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
201	# length of .L._println_str0
202		.int 0
203	.L._println_str0:
204		.asciz ""
205	.text
206	_println:
207		pushq %rbp
208		movq %rsp, %rbp
209		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
210		andq $-16, %rsp
211		leaq .L._println_str0(%rip), %rdi
212		call puts@plt
213		movq $0, %rdi
214		call fflush@plt
215		movq %rbp, %rsp
216		popq %rbp
217		ret
218	
219	_exit:
220		pushq %rbp
221		movq %rsp, %rbp
222		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
223		andq $-16, %rsp
224		call exit@plt
225		movq %rbp, %rsp
226		popq %rbp
227		ret
228	
229	.section .rodata
230	# length of .L._errOverflow_str0
231		.int 52
232	.L._errOverflow_str0:
233		.asciz "fatal error: integer overflow or underflow occurred\n"
234	.text
235	_errOverflow:
236		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
237		andq $-16, %rsp
238		leaq .L._errOverflow_str0(%rip), %rdi
239		call _prints
240		movb $-1, %dil
241		call exit@plt
===========================================================
-- Finished

