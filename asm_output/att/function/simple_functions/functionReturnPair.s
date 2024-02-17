./valid/function/simple_functions/functionReturnPair.wacc
calling the reference compiler on ./valid/function/simple_functions/functionReturnPair.wacc
-- Test: functionReturnPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# creates a pair which is returned from a function

# Output:
# 10
#

# Program:

begin

  pair(int, int) getPair() is
    pair(int, int) p = newpair(10,15);
    return p
  end

  pair(int, int) p = call getPair();
  int x = fst p;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionReturnPair.s contents are:
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
12		# Stack pointer unchanged, no stack allocated arguments
13		call wacc_getPair
14		movq %rax, %r11
15		# Stack pointer unchanged, no stack allocated arguments
16		movq %r11, %rax
17		movq %rax, %r12
18		cmpq $0, %r12
19		je _errNull
20		movq (%r12), %rax
21		movq %rax, %rax
22		movq %rax, %r13
23		# Stack pointer unchanged, no stack allocated arguments
24		movq %r13, %rax
25		movq %rax, %rdi
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		movq $0, %rax
31		# popq {%rbx, %r12, %r13}
32		movq (%rsp), %rbx
33		movq 8(%rsp), %r12
34		movq 16(%rsp), %r13
35		addq $24, %rsp
36		popq %rbp
37		ret
38	
39	wacc_getPair:
40		pushq %rbp
41		pushq %r12
42		movq %rsp, %rbp
43		# Stack pointer unchanged, no stack allocated variables
44		movl $16, %edi
45		call _malloc
46		movq %rax, %r11
47		movq $10, %rax
48		movq %rax, (%r11)
49		movq $15, %rax
50		movq %rax, 8(%r11)
51		movq %r11, %rax
52		movq %rax, %r12
53		movq %r12, %rax
54		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
55		movq %rbp, %rsp
56		popq %r12
57		popq %rbp
58		ret
59		# Stack pointer unchanged, no stack allocated variables
60		# 'ere be dragons: this is 100% dead code, functions always end in returns!
61	
62	.section .rodata
63	# length of .L._prints_str0
64		.int 4
65	.L._prints_str0:
66		.asciz "%.*s"
67	.text
68	_prints:
69		pushq %rbp
70		movq %rsp, %rbp
71		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
72		andq $-16, %rsp
73		movq %rdi, %rdx
74		movl -4(%rdi), %esi
75		leaq .L._prints_str0(%rip), %rdi
76		# on x86, al represents the number of SIMD registers used as variadic arguments
77		movb $0, %al
78		call printf@plt
79		movq $0, %rdi
80		call fflush@plt
81		movq %rbp, %rsp
82		popq %rbp
83		ret
84	
85	_malloc:
86		pushq %rbp
87		movq %rsp, %rbp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		andq $-16, %rsp
90		call malloc@plt
91		cmpq $0, %rax
92		je _errOutOfMemory
93		movq %rbp, %rsp
94		popq %rbp
95		ret
96	
97	.section .rodata
98	# length of .L._printi_str0
99		.int 2
100	.L._printi_str0:
101		.asciz "%d"
102	.text
103	_printi:
104		pushq %rbp
105		movq %rsp, %rbp
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		andq $-16, %rsp
108		movl %edi, %esi
109		leaq .L._printi_str0(%rip), %rdi
110		# on x86, al represents the number of SIMD registers used as variadic arguments
111		movb $0, %al
112		call printf@plt
113		movq $0, %rdi
114		call fflush@plt
115		movq %rbp, %rsp
116		popq %rbp
117		ret
118	
119	.section .rodata
120	# length of .L._println_str0
121		.int 0
122	.L._println_str0:
123		.asciz ""
124	.text
125	_println:
126		pushq %rbp
127		movq %rsp, %rbp
128		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
129		andq $-16, %rsp
130		leaq .L._println_str0(%rip), %rdi
131		call puts@plt
132		movq $0, %rdi
133		call fflush@plt
134		movq %rbp, %rsp
135		popq %rbp
136		ret
137	
138	_exit:
139		pushq %rbp
140		movq %rsp, %rbp
141		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
142		andq $-16, %rsp
143		call exit@plt
144		movq %rbp, %rsp
145		popq %rbp
146		ret
147	
148	.section .rodata
149	# length of .L._errOutOfMemory_str0
150		.int 27
151	.L._errOutOfMemory_str0:
152		.asciz "fatal error: out of memory\n"
153	.text
154	_errOutOfMemory:
155		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
156		andq $-16, %rsp
157		leaq .L._errOutOfMemory_str0(%rip), %rdi
158		call _prints
159		movb $-1, %dil
160		call exit@plt
161	
162	.section .rodata
163	# length of .L._errNull_str0
164		.int 45
165	.L._errNull_str0:
166		.asciz "fatal error: null pair dereferenced or freed\n"
167	.text
168	_errNull:
169		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
170		andq $-16, %rsp
171		leaq .L._errNull_str0(%rip), %rdi
172		call _prints
173		movb $-1, %dil
174		call exit@plt
===========================================================
-- Finished

