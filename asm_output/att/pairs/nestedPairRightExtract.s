./valid/pairs/nestedPairRightExtract.wacc
calling the reference compiler on ./valid/pairs/nestedPairRightExtract.wacc
-- Test: nestedPairRightExtract.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair extractions are legal as long as the left hand-side type is known

# Output:
# 2
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  int x = fst snd q ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairRightExtract.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14}
6		subq $32, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %rsp, %rbp
12		# Stack pointer unchanged, no stack allocated variables
13		movl $16, %edi
14		call _malloc
15		movq %rax, %r11
16		movq $2, %rax
17		movq %rax, (%r11)
18		movq $3, %rax
19		movq %rax, 8(%r11)
20		movq %r11, %rax
21		movq %rax, %r12
22		movl $16, %edi
23		call _malloc
24		movq %rax, %r11
25		movq $1, %rax
26		movq %rax, (%r11)
27		movq %r12, %rax
28		movq %rax, 8(%r11)
29		movq %r11, %rax
30		movq %rax, %r13
31		cmpq $0, %r13
32		je _errNull
33		movq 8(%r13), %rax
34		movq %rax, %rax
35		pushq %rax
36		popq %rax
37		cmpq $0, %rax
38		je _errNull
39		movq (%rax), %rax
40		movq %rax, %rax
41		movq %rax, %r14
42		# Stack pointer unchanged, no stack allocated arguments
43		movq %r14, %rax
44		movq %rax, %rdi
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _printi
47		call _println
48		# Stack pointer unchanged, no stack allocated variables
49		movq $0, %rax
50		# popq {%rbx, %r12, %r13, %r14}
51		movq (%rsp), %rbx
52		movq 8(%rsp), %r12
53		movq 16(%rsp), %r13
54		movq 24(%rsp), %r14
55		addq $32, %rsp
56		popq %rbp
57		ret
58	
59	.section .rodata
60	# length of .L._prints_str0
61		.int 4
62	.L._prints_str0:
63		.asciz "%.*s"
64	.text
65	_prints:
66		pushq %rbp
67		movq %rsp, %rbp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		andq $-16, %rsp
70		movq %rdi, %rdx
71		movl -4(%rdi), %esi
72		leaq .L._prints_str0(%rip), %rdi
73		# on x86, al represents the number of SIMD registers used as variadic arguments
74		movb $0, %al
75		call printf@plt
76		movq $0, %rdi
77		call fflush@plt
78		movq %rbp, %rsp
79		popq %rbp
80		ret
81	
82	_malloc:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		call malloc@plt
88		cmpq $0, %rax
89		je _errOutOfMemory
90		movq %rbp, %rsp
91		popq %rbp
92		ret
93	
94	.section .rodata
95	# length of .L._printi_str0
96		.int 2
97	.L._printi_str0:
98		.asciz "%d"
99	.text
100	_printi:
101		pushq %rbp
102		movq %rsp, %rbp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		andq $-16, %rsp
105		movl %edi, %esi
106		leaq .L._printi_str0(%rip), %rdi
107		# on x86, al represents the number of SIMD registers used as variadic arguments
108		movb $0, %al
109		call printf@plt
110		movq $0, %rdi
111		call fflush@plt
112		movq %rbp, %rsp
113		popq %rbp
114		ret
115	
116	.section .rodata
117	# length of .L._println_str0
118		.int 0
119	.L._println_str0:
120		.asciz ""
121	.text
122	_println:
123		pushq %rbp
124		movq %rsp, %rbp
125		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
126		andq $-16, %rsp
127		leaq .L._println_str0(%rip), %rdi
128		call puts@plt
129		movq $0, %rdi
130		call fflush@plt
131		movq %rbp, %rsp
132		popq %rbp
133		ret
134	
135	_exit:
136		pushq %rbp
137		movq %rsp, %rbp
138		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
139		andq $-16, %rsp
140		call exit@plt
141		movq %rbp, %rsp
142		popq %rbp
143		ret
144	
145	.section .rodata
146	# length of .L._errOutOfMemory_str0
147		.int 27
148	.L._errOutOfMemory_str0:
149		.asciz "fatal error: out of memory\n"
150	.text
151	_errOutOfMemory:
152		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
153		andq $-16, %rsp
154		leaq .L._errOutOfMemory_str0(%rip), %rdi
155		call _prints
156		movb $-1, %dil
157		call exit@plt
158	
159	.section .rodata
160	# length of .L._errNull_str0
161		.int 45
162	.L._errNull_str0:
163		.asciz "fatal error: null pair dereferenced or freed\n"
164	.text
165	_errNull:
166		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
167		andq $-16, %rsp
168		leaq .L._errNull_str0(%rip), %rdi
169		call _prints
170		movb $-1, %dil
171		call exit@plt
===========================================================
-- Finished

