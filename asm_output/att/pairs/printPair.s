./valid/pairs/printPair.wacc
calling the reference compiler on ./valid/pairs/printPair.wacc
-- Test: printPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair program

# Output:
# #addrs# = (10, a)
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  print p ;
  print " = (" ;
  int x = fst p ;
  print x ;
  print ", " ;
  char c = snd p ;
  print c ;
  println ')'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPair.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 4
4	.L.str0:
5		.asciz " = ("
6	# length of .L.str1
7		.int 2
8	.L.str1:
9		.asciz ", "
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12, %r13, %r14}
14		subq $32, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %r13, 16(%rsp)
18		movq %r14, 24(%rsp)
19		movq %rsp, %rbp
20		# Stack pointer unchanged, no stack allocated variables
21		movl $16, %edi
22		call _malloc
23		movq %rax, %r11
24		movq $10, %rax
25		movq %rax, (%r11)
26		movq $97, %rax
27		movq %rax, 8(%r11)
28		movq %r11, %rax
29		movq %rax, %r12
30		# Stack pointer unchanged, no stack allocated arguments
31		movq %r12, %rax
32		movq %rax, %rdi
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _printp
35		# Stack pointer unchanged, no stack allocated arguments
36		leaq .L.str0(%rip), %rax
37		pushq %rax
38		popq %rax
39		movq %rax, %rax
40		movq %rax, %rdi
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _prints
43		cmpq $0, %r12
44		je _errNull
45		movq (%r12), %rax
46		movq %rax, %rax
47		movq %rax, %r13
48		# Stack pointer unchanged, no stack allocated arguments
49		movq %r13, %rax
50		movq %rax, %rdi
51		# statement primitives do not return results (but will clobber r0/rax)
52		call _printi
53		# Stack pointer unchanged, no stack allocated arguments
54		leaq .L.str1(%rip), %rax
55		pushq %rax
56		popq %rax
57		movq %rax, %rax
58		movq %rax, %rdi
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _prints
61		cmpq $0, %r12
62		je _errNull
63		movq 8(%r12), %rax
64		movq %rax, %rax
65		movq %rax, %r14
66		# Stack pointer unchanged, no stack allocated arguments
67		movq %r14, %rax
68		movq %rax, %rdi
69		# statement primitives do not return results (but will clobber r0/rax)
70		call _printc
71		# Stack pointer unchanged, no stack allocated arguments
72		movq $41, %rax
73		movq %rax, %rdi
74		# statement primitives do not return results (but will clobber r0/rax)
75		call _printc
76		call _println
77		# Stack pointer unchanged, no stack allocated variables
78		movq $0, %rax
79		# popq {%rbx, %r12, %r13, %r14}
80		movq (%rsp), %rbx
81		movq 8(%rsp), %r12
82		movq 16(%rsp), %r13
83		movq 24(%rsp), %r14
84		addq $32, %rsp
85		popq %rbp
86		ret
87	
88	.section .rodata
89	# length of .L._prints_str0
90		.int 4
91	.L._prints_str0:
92		.asciz "%.*s"
93	.text
94	_prints:
95		pushq %rbp
96		movq %rsp, %rbp
97		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
98		andq $-16, %rsp
99		movq %rdi, %rdx
100		movl -4(%rdi), %esi
101		leaq .L._prints_str0(%rip), %rdi
102		# on x86, al represents the number of SIMD registers used as variadic arguments
103		movb $0, %al
104		call printf@plt
105		movq $0, %rdi
106		call fflush@plt
107		movq %rbp, %rsp
108		popq %rbp
109		ret
110	
111	.section .rodata
112	# length of .L._printp_str0
113		.int 2
114	.L._printp_str0:
115		.asciz "%p"
116	.text
117	_printp:
118		pushq %rbp
119		movq %rsp, %rbp
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		andq $-16, %rsp
122		movq %rdi, %rsi
123		leaq .L._printp_str0(%rip), %rdi
124		# on x86, al represents the number of SIMD registers used as variadic arguments
125		movb $0, %al
126		call printf@plt
127		movq $0, %rdi
128		call fflush@plt
129		movq %rbp, %rsp
130		popq %rbp
131		ret
132	
133	_malloc:
134		pushq %rbp
135		movq %rsp, %rbp
136		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
137		andq $-16, %rsp
138		call malloc@plt
139		cmpq $0, %rax
140		je _errOutOfMemory
141		movq %rbp, %rsp
142		popq %rbp
143		ret
144	
145	.section .rodata
146	# length of .L._printi_str0
147		.int 2
148	.L._printi_str0:
149		.asciz "%d"
150	.text
151	_printi:
152		pushq %rbp
153		movq %rsp, %rbp
154		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
155		andq $-16, %rsp
156		movl %edi, %esi
157		leaq .L._printi_str0(%rip), %rdi
158		# on x86, al represents the number of SIMD registers used as variadic arguments
159		movb $0, %al
160		call printf@plt
161		movq $0, %rdi
162		call fflush@plt
163		movq %rbp, %rsp
164		popq %rbp
165		ret
166	
167	.section .rodata
168	# length of .L._printc_str0
169		.int 2
170	.L._printc_str0:
171		.asciz "%c"
172	.text
173	_printc:
174		pushq %rbp
175		movq %rsp, %rbp
176		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
177		andq $-16, %rsp
178		movb %dil, %sil
179		leaq .L._printc_str0(%rip), %rdi
180		# on x86, al represents the number of SIMD registers used as variadic arguments
181		movb $0, %al
182		call printf@plt
183		movq $0, %rdi
184		call fflush@plt
185		movq %rbp, %rsp
186		popq %rbp
187		ret
188	
189	.section .rodata
190	# length of .L._println_str0
191		.int 0
192	.L._println_str0:
193		.asciz ""
194	.text
195	_println:
196		pushq %rbp
197		movq %rsp, %rbp
198		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
199		andq $-16, %rsp
200		leaq .L._println_str0(%rip), %rdi
201		call puts@plt
202		movq $0, %rdi
203		call fflush@plt
204		movq %rbp, %rsp
205		popq %rbp
206		ret
207	
208	_exit:
209		pushq %rbp
210		movq %rsp, %rbp
211		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
212		andq $-16, %rsp
213		call exit@plt
214		movq %rbp, %rsp
215		popq %rbp
216		ret
217	
218	.section .rodata
219	# length of .L._errOutOfMemory_str0
220		.int 27
221	.L._errOutOfMemory_str0:
222		.asciz "fatal error: out of memory\n"
223	.text
224	_errOutOfMemory:
225		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
226		andq $-16, %rsp
227		leaq .L._errOutOfMemory_str0(%rip), %rdi
228		call _prints
229		movb $-1, %dil
230		call exit@plt
231	
232	.section .rodata
233	# length of .L._errNull_str0
234		.int 45
235	.L._errNull_str0:
236		.asciz "fatal error: null pair dereferenced or freed\n"
237	.text
238	_errNull:
239		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
240		andq $-16, %rsp
241		leaq .L._errNull_str0(%rip), %rdi
242		call _prints
243		movb $-1, %dil
244		call exit@plt
===========================================================
-- Finished

