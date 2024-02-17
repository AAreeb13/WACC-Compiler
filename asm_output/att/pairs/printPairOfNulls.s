./valid/pairs/printPairOfNulls.wacc
calling the reference compiler on ./valid/pairs/printPairOfNulls.wacc
-- Test: printPairOfNulls.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a pair of null pairs

# Output:
# #addrs# = ((nil),(nil))
#

# Program:

begin
  pair(pair, pair) p = newpair(null, null) ;
  print p ;
  print " = (" ;
  pair(pair, pair) q = fst p ;
  print q ;
  print "," ;
  pair(int, bool) r = snd p ;
  print r ;
  println ")"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPairOfNulls.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 4
4	.L.str0:
5		.asciz " = ("
6	# length of .L.str1
7		.int 1
8	.L.str1:
9		.asciz ","
10	# length of .L.str2
11		.int 1
12	.L.str2:
13		.asciz ")"
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12, %r13, %r14}
18		subq $32, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %r14, 24(%rsp)
23		movq %rsp, %rbp
24		# Stack pointer unchanged, no stack allocated variables
25		movl $16, %edi
26		call _malloc
27		movq %rax, %r11
28		movq $0, %rax
29		movq %rax, (%r11)
30		movq $0, %rax
31		movq %rax, 8(%r11)
32		movq %r11, %rax
33		movq %rax, %r12
34		# Stack pointer unchanged, no stack allocated arguments
35		movq %r12, %rax
36		movq %rax, %rdi
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _printp
39		# Stack pointer unchanged, no stack allocated arguments
40		leaq .L.str0(%rip), %rax
41		pushq %rax
42		popq %rax
43		movq %rax, %rax
44		movq %rax, %rdi
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _prints
47		cmpq $0, %r12
48		je _errNull
49		movq (%r12), %rax
50		movq %rax, %rax
51		movq %rax, %r13
52		# Stack pointer unchanged, no stack allocated arguments
53		movq %r13, %rax
54		movq %rax, %rdi
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _printp
57		# Stack pointer unchanged, no stack allocated arguments
58		leaq .L.str1(%rip), %rax
59		pushq %rax
60		popq %rax
61		movq %rax, %rax
62		movq %rax, %rdi
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _prints
65		cmpq $0, %r12
66		je _errNull
67		movq 8(%r12), %rax
68		movq %rax, %rax
69		movq %rax, %r14
70		# Stack pointer unchanged, no stack allocated arguments
71		movq %r14, %rax
72		movq %rax, %rdi
73		# statement primitives do not return results (but will clobber r0/rax)
74		call _printp
75		# Stack pointer unchanged, no stack allocated arguments
76		leaq .L.str2(%rip), %rax
77		pushq %rax
78		popq %rax
79		movq %rax, %rax
80		movq %rax, %rdi
81		# statement primitives do not return results (but will clobber r0/rax)
82		call _prints
83		call _println
84		# Stack pointer unchanged, no stack allocated variables
85		movq $0, %rax
86		# popq {%rbx, %r12, %r13, %r14}
87		movq (%rsp), %rbx
88		movq 8(%rsp), %r12
89		movq 16(%rsp), %r13
90		movq 24(%rsp), %r14
91		addq $32, %rsp
92		popq %rbp
93		ret
94	
95	.section .rodata
96	# length of .L._prints_str0
97		.int 4
98	.L._prints_str0:
99		.asciz "%.*s"
100	.text
101	_prints:
102		pushq %rbp
103		movq %rsp, %rbp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		andq $-16, %rsp
106		movq %rdi, %rdx
107		movl -4(%rdi), %esi
108		leaq .L._prints_str0(%rip), %rdi
109		# on x86, al represents the number of SIMD registers used as variadic arguments
110		movb $0, %al
111		call printf@plt
112		movq $0, %rdi
113		call fflush@plt
114		movq %rbp, %rsp
115		popq %rbp
116		ret
117	
118	.section .rodata
119	# length of .L._printp_str0
120		.int 2
121	.L._printp_str0:
122		.asciz "%p"
123	.text
124	_printp:
125		pushq %rbp
126		movq %rsp, %rbp
127		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
128		andq $-16, %rsp
129		movq %rdi, %rsi
130		leaq .L._printp_str0(%rip), %rdi
131		# on x86, al represents the number of SIMD registers used as variadic arguments
132		movb $0, %al
133		call printf@plt
134		movq $0, %rdi
135		call fflush@plt
136		movq %rbp, %rsp
137		popq %rbp
138		ret
139	
140	_malloc:
141		pushq %rbp
142		movq %rsp, %rbp
143		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
144		andq $-16, %rsp
145		call malloc@plt
146		cmpq $0, %rax
147		je _errOutOfMemory
148		movq %rbp, %rsp
149		popq %rbp
150		ret
151	
152	.section .rodata
153	# length of .L._println_str0
154		.int 0
155	.L._println_str0:
156		.asciz ""
157	.text
158	_println:
159		pushq %rbp
160		movq %rsp, %rbp
161		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
162		andq $-16, %rsp
163		leaq .L._println_str0(%rip), %rdi
164		call puts@plt
165		movq $0, %rdi
166		call fflush@plt
167		movq %rbp, %rsp
168		popq %rbp
169		ret
170	
171	_exit:
172		pushq %rbp
173		movq %rsp, %rbp
174		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
175		andq $-16, %rsp
176		call exit@plt
177		movq %rbp, %rsp
178		popq %rbp
179		ret
180	
181	.section .rodata
182	# length of .L._errOutOfMemory_str0
183		.int 27
184	.L._errOutOfMemory_str0:
185		.asciz "fatal error: out of memory\n"
186	.text
187	_errOutOfMemory:
188		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
189		andq $-16, %rsp
190		leaq .L._errOutOfMemory_str0(%rip), %rdi
191		call _prints
192		movb $-1, %dil
193		call exit@plt
194	
195	.section .rodata
196	# length of .L._errNull_str0
197		.int 45
198	.L._errNull_str0:
199		.asciz "fatal error: null pair dereferenced or freed\n"
200	.text
201	_errNull:
202		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
203		andq $-16, %rsp
204		leaq .L._errNull_str0(%rip), %rdi
205		call _prints
206		movb $-1, %dil
207		call exit@plt
===========================================================
-- Finished

