./valid/pairs/nestedPairLeftAssign.wacc
calling the reference compiler on ./valid/pairs/nestedPairLeftAssign.wacc
-- Test: nestedPairLeftAssign.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair assignments are legal as long as the right hand-side type is known

# Output:
# 7
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  fst snd q = 7 ;
  int x = fst p ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairLeftAssign.s contents are:
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
36		popq %rbx
37		cmpq $0, %rbx
38		je _errNull
39		movq $7, %rax
40		movq %rax, (%rbx)
41		cmpq $0, %r12
42		je _errNull
43		movq (%r12), %rax
44		movq %rax, %rax
45		movq %rax, %r14
46		# Stack pointer unchanged, no stack allocated arguments
47		movq %r14, %rax
48		movq %rax, %rdi
49		# statement primitives do not return results (but will clobber r0/rax)
50		call _printi
51		call _println
52		# Stack pointer unchanged, no stack allocated variables
53		movq $0, %rax
54		# popq {%rbx, %r12, %r13, %r14}
55		movq (%rsp), %rbx
56		movq 8(%rsp), %r12
57		movq 16(%rsp), %r13
58		movq 24(%rsp), %r14
59		addq $32, %rsp
60		popq %rbp
61		ret
62	
63	.section .rodata
64	# length of .L._prints_str0
65		.int 4
66	.L._prints_str0:
67		.asciz "%.*s"
68	.text
69	_prints:
70		pushq %rbp
71		movq %rsp, %rbp
72		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
73		andq $-16, %rsp
74		movq %rdi, %rdx
75		movl -4(%rdi), %esi
76		leaq .L._prints_str0(%rip), %rdi
77		# on x86, al represents the number of SIMD registers used as variadic arguments
78		movb $0, %al
79		call printf@plt
80		movq $0, %rdi
81		call fflush@plt
82		movq %rbp, %rsp
83		popq %rbp
84		ret
85	
86	_malloc:
87		pushq %rbp
88		movq %rsp, %rbp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		andq $-16, %rsp
91		call malloc@plt
92		cmpq $0, %rax
93		je _errOutOfMemory
94		movq %rbp, %rsp
95		popq %rbp
96		ret
97	
98	.section .rodata
99	# length of .L._printi_str0
100		.int 2
101	.L._printi_str0:
102		.asciz "%d"
103	.text
104	_printi:
105		pushq %rbp
106		movq %rsp, %rbp
107		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
108		andq $-16, %rsp
109		movl %edi, %esi
110		leaq .L._printi_str0(%rip), %rdi
111		# on x86, al represents the number of SIMD registers used as variadic arguments
112		movb $0, %al
113		call printf@plt
114		movq $0, %rdi
115		call fflush@plt
116		movq %rbp, %rsp
117		popq %rbp
118		ret
119	
120	.section .rodata
121	# length of .L._println_str0
122		.int 0
123	.L._println_str0:
124		.asciz ""
125	.text
126	_println:
127		pushq %rbp
128		movq %rsp, %rbp
129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
130		andq $-16, %rsp
131		leaq .L._println_str0(%rip), %rdi
132		call puts@plt
133		movq $0, %rdi
134		call fflush@plt
135		movq %rbp, %rsp
136		popq %rbp
137		ret
138	
139	_exit:
140		pushq %rbp
141		movq %rsp, %rbp
142		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
143		andq $-16, %rsp
144		call exit@plt
145		movq %rbp, %rsp
146		popq %rbp
147		ret
148	
149	.section .rodata
150	# length of .L._errOutOfMemory_str0
151		.int 27
152	.L._errOutOfMemory_str0:
153		.asciz "fatal error: out of memory\n"
154	.text
155	_errOutOfMemory:
156		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
157		andq $-16, %rsp
158		leaq .L._errOutOfMemory_str0(%rip), %rdi
159		call _prints
160		movb $-1, %dil
161		call exit@plt
162	
163	.section .rodata
164	# length of .L._errNull_str0
165		.int 45
166	.L._errNull_str0:
167		.asciz "fatal error: null pair dereferenced or freed\n"
168	.text
169	_errNull:
170		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
171		andq $-16, %rsp
172		leaq .L._errNull_str0(%rip), %rdi
173		call _prints
174		movb $-1, %dil
175		call exit@plt
===========================================================
-- Finished

