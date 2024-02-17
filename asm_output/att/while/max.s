./valid/while/max.wacc
calling the reference compiler on ./valid/while/max.wacc
-- Test: max.wacc

-- Uploaded file: 
---------------------------------------------------------------
# find the max of two numbers

# Output:
# max value = 17
#

# Program:

begin
  int i = 0 ;
  int x = 10 ;
  int y = 17 ;
  while (y > 0 || x > 0) do
    x = x - 1 ;
    y = y - 1 ;
    i = i + 1
  done ;
  print "max value = ";
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
max.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 12
4	.L.str0:
5		.asciz "max value = "
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12, %r13, %r14}
10		subq $32, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %r13, 16(%rsp)
14		movq %r14, 24(%rsp)
15		movq %rsp, %rbp
16		# Stack pointer unchanged, no stack allocated variables
17		movq $0, %rax
18		movq %rax, %r12
19		movq $10, %rax
20		movq %rax, %r13
21		movq $17, %rax
22		movq %rax, %r14
23		jmp .L0
24	.L1:
25		movl %r13d, %eax
26		subl $1, %eax
27		jo _errOverflow
28		movslq %eax, %rax
29		pushq %rax
30		popq %rax
31		movq %rax, %rax
32		movq %rax, %r13
33		movl %r14d, %eax
34		subl $1, %eax
35		jo _errOverflow
36		movslq %eax, %rax
37		pushq %rax
38		popq %rax
39		movq %rax, %rax
40		movq %rax, %r14
41		movl %r12d, %eax
42		addl $1, %eax
43		jo _errOverflow
44		movslq %eax, %rax
45		pushq %rax
46		popq %rax
47		movq %rax, %rax
48		movq %rax, %r12
49	.L0:
50		cmpq $0, %r14
51		setg %al
52		movsbq %al, %rax
53		pushq %rax
54		popq %rax
55		cmpq $1, %rax
56		je .L2
57		cmpq $0, %r13
58		setg %al
59		movsbq %al, %rax
60		pushq %rax
61		popq %rax
62		cmpq $1, %rax
63	.L2:
64		je .L1
65		# Stack pointer unchanged, no stack allocated arguments
66		leaq .L.str0(%rip), %rax
67		pushq %rax
68		popq %rax
69		movq %rax, %rax
70		movq %rax, %rdi
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _prints
73		# Stack pointer unchanged, no stack allocated arguments
74		movq %r12, %rax
75		movq %rax, %rdi
76		# statement primitives do not return results (but will clobber r0/rax)
77		call _printi
78		call _println
79		# Stack pointer unchanged, no stack allocated variables
80		movq $0, %rax
81		# popq {%rbx, %r12, %r13, %r14}
82		movq (%rsp), %rbx
83		movq 8(%rsp), %r12
84		movq 16(%rsp), %r13
85		movq 24(%rsp), %r14
86		addq $32, %rsp
87		popq %rbp
88		ret
89	
90	.section .rodata
91	# length of .L._prints_str0
92		.int 4
93	.L._prints_str0:
94		.asciz "%.*s"
95	.text
96	_prints:
97		pushq %rbp
98		movq %rsp, %rbp
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		andq $-16, %rsp
101		movq %rdi, %rdx
102		movl -4(%rdi), %esi
103		leaq .L._prints_str0(%rip), %rdi
104		# on x86, al represents the number of SIMD registers used as variadic arguments
105		movb $0, %al
106		call printf@plt
107		movq $0, %rdi
108		call fflush@plt
109		movq %rbp, %rsp
110		popq %rbp
111		ret
112	
113	.section .rodata
114	# length of .L._printi_str0
115		.int 2
116	.L._printi_str0:
117		.asciz "%d"
118	.text
119	_printi:
120		pushq %rbp
121		movq %rsp, %rbp
122		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
123		andq $-16, %rsp
124		movl %edi, %esi
125		leaq .L._printi_str0(%rip), %rdi
126		# on x86, al represents the number of SIMD registers used as variadic arguments
127		movb $0, %al
128		call printf@plt
129		movq $0, %rdi
130		call fflush@plt
131		movq %rbp, %rsp
132		popq %rbp
133		ret
134	
135	.section .rodata
136	# length of .L._println_str0
137		.int 0
138	.L._println_str0:
139		.asciz ""
140	.text
141	_println:
142		pushq %rbp
143		movq %rsp, %rbp
144		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
145		andq $-16, %rsp
146		leaq .L._println_str0(%rip), %rdi
147		call puts@plt
148		movq $0, %rdi
149		call fflush@plt
150		movq %rbp, %rsp
151		popq %rbp
152		ret
153	
154	_exit:
155		pushq %rbp
156		movq %rsp, %rbp
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		andq $-16, %rsp
159		call exit@plt
160		movq %rbp, %rsp
161		popq %rbp
162		ret
163	
164	.section .rodata
165	# length of .L._errOverflow_str0
166		.int 52
167	.L._errOverflow_str0:
168		.asciz "fatal error: integer overflow or underflow occurred\n"
169	.text
170	_errOverflow:
171		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
172		andq $-16, %rsp
173		leaq .L._errOverflow_str0(%rip), %rdi
174		call _prints
175		movb $-1, %dil
176		call exit@plt
===========================================================
-- Finished

