./valid/pairs/writeSnd.wacc
calling the reference compiler on ./valid/pairs/writeSnd.wacc
-- Test: writeSnd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair and write to its second element

# Output:
# a
# Z
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  char s = snd p ;
  println s ;
  snd p = 'Z' ;
  s = snd p ;
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
writeSnd.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13}
6		subq $24, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated variables
12		movl $16, %edi
13		call _malloc
14		movq %rax, %r11
15		movq $10, %rax
16		movq %rax, (%r11)
17		movq $97, %rax
18		movq %rax, 8(%r11)
19		movq %r11, %rax
20		movq %rax, %r12
21		cmpq $0, %r12
22		je _errNull
23		movq 8(%r12), %rax
24		movq %rax, %rax
25		movq %rax, %r13
26		# Stack pointer unchanged, no stack allocated arguments
27		movq %r13, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _printc
31		call _println
32		cmpq $0, %r12
33		je _errNull
34		movq $90, %rax
35		movq %rax, 8(%r12)
36		cmpq $0, %r12
37		je _errNull
38		movq 8(%r12), %rax
39		movq %rax, %rax
40		pushq %rax
41		popq %rax
42		movq %rax, %rax
43		movq %rax, %r13
44		# Stack pointer unchanged, no stack allocated arguments
45		movq %r13, %rax
46		movq %rax, %rdi
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _printc
49		call _println
50		# Stack pointer unchanged, no stack allocated variables
51		movq $0, %rax
52		# popq {%rbx, %r12, %r13}
53		movq (%rsp), %rbx
54		movq 8(%rsp), %r12
55		movq 16(%rsp), %r13
56		addq $24, %rsp
57		popq %rbp
58		ret
59	
60	.section .rodata
61	# length of .L._prints_str0
62		.int 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.text
66	_prints:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		movq %rdi, %rdx
72		movl -4(%rdi), %esi
73		leaq .L._prints_str0(%rip), %rdi
74		# on x86, al represents the number of SIMD registers used as variadic arguments
75		movb $0, %al
76		call printf@plt
77		movq $0, %rdi
78		call fflush@plt
79		movq %rbp, %rsp
80		popq %rbp
81		ret
82	
83	_malloc:
84		pushq %rbp
85		movq %rsp, %rbp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		andq $-16, %rsp
88		call malloc@plt
89		cmpq $0, %rax
90		je _errOutOfMemory
91		movq %rbp, %rsp
92		popq %rbp
93		ret
94	
95	.section .rodata
96	# length of .L._printc_str0
97		.int 2
98	.L._printc_str0:
99		.asciz "%c"
100	.text
101	_printc:
102		pushq %rbp
103		movq %rsp, %rbp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		andq $-16, %rsp
106		movb %dil, %sil
107		leaq .L._printc_str0(%rip), %rdi
108		# on x86, al represents the number of SIMD registers used as variadic arguments
109		movb $0, %al
110		call printf@plt
111		movq $0, %rdi
112		call fflush@plt
113		movq %rbp, %rsp
114		popq %rbp
115		ret
116	
117	.section .rodata
118	# length of .L._println_str0
119		.int 0
120	.L._println_str0:
121		.asciz ""
122	.text
123	_println:
124		pushq %rbp
125		movq %rsp, %rbp
126		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
127		andq $-16, %rsp
128		leaq .L._println_str0(%rip), %rdi
129		call puts@plt
130		movq $0, %rdi
131		call fflush@plt
132		movq %rbp, %rsp
133		popq %rbp
134		ret
135	
136	_exit:
137		pushq %rbp
138		movq %rsp, %rbp
139		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
140		andq $-16, %rsp
141		call exit@plt
142		movq %rbp, %rsp
143		popq %rbp
144		ret
145	
146	.section .rodata
147	# length of .L._errOutOfMemory_str0
148		.int 27
149	.L._errOutOfMemory_str0:
150		.asciz "fatal error: out of memory\n"
151	.text
152	_errOutOfMemory:
153		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
154		andq $-16, %rsp
155		leaq .L._errOutOfMemory_str0(%rip), %rdi
156		call _prints
157		movb $-1, %dil
158		call exit@plt
159	
160	.section .rodata
161	# length of .L._errNull_str0
162		.int 45
163	.L._errNull_str0:
164		.asciz "fatal error: null pair dereferenced or freed\n"
165	.text
166	_errNull:
167		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
168		andq $-16, %rsp
169		leaq .L._errNull_str0(%rip), %rdi
170		call _prints
171		movb $-1, %dil
172		call exit@plt
===========================================================
-- Finished

