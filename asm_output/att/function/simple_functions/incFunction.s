./valid/function/simple_functions/incFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/incFunction.wacc
-- Test: incFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple increment function definition and usage

# Output:
# 1
# 4
#

# Program:

begin
  int inc(int x) is
    return x + 1
  end
  int x = 0 ;
  x = call inc(x) ;
  println x ;
  x = call inc(x) ;
  x = call inc(x) ;
  x = call inc(x) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
incFunction.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12}
6		subq $16, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %rsp, %rbp
10		# Stack pointer unchanged, no stack allocated variables
11		movq $0, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		call wacc_inc
17		movq %rax, %r11
18		# Stack pointer unchanged, no stack allocated arguments
19		movq %r11, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated arguments
22		movq %r12, %rax
23		movq %rax, %rdi
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _printi
26		call _println
27		# Stack pointer unchanged, no stack allocated arguments
28		movq %r12, %rax
29		movq %rax, %rdi
30		call wacc_inc
31		movq %rax, %r11
32		# Stack pointer unchanged, no stack allocated arguments
33		movq %r11, %rax
34		movq %rax, %r12
35		# Stack pointer unchanged, no stack allocated arguments
36		movq %r12, %rax
37		movq %rax, %rdi
38		call wacc_inc
39		movq %rax, %r11
40		# Stack pointer unchanged, no stack allocated arguments
41		movq %r11, %rax
42		movq %rax, %r12
43		# Stack pointer unchanged, no stack allocated arguments
44		movq %r12, %rax
45		movq %rax, %rdi
46		call wacc_inc
47		movq %rax, %r11
48		# Stack pointer unchanged, no stack allocated arguments
49		movq %r11, %rax
50		movq %rax, %r12
51		# Stack pointer unchanged, no stack allocated arguments
52		movq %r12, %rax
53		movq %rax, %rdi
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _printi
56		call _println
57		# Stack pointer unchanged, no stack allocated variables
58		movq $0, %rax
59		# popq {%rbx, %r12}
60		movq (%rsp), %rbx
61		movq 8(%rsp), %r12
62		addq $16, %rsp
63		popq %rbp
64		ret
65	
66	wacc_inc:
67		pushq %rbp
68		movq %rsp, %rbp
69		movl %edi, %eax
70		addl $1, %eax
71		jo _errOverflow
72		movslq %eax, %rax
73		pushq %rax
74		popq %rax
75		movq %rax, %rax
76		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
77		movq %rbp, %rsp
78		popq %rbp
79		ret
80		# 'ere be dragons: this is 100% dead code, functions always end in returns!
81	
82	.section .rodata
83	# length of .L._prints_str0
84		.int 4
85	.L._prints_str0:
86		.asciz "%.*s"
87	.text
88	_prints:
89		pushq %rbp
90		movq %rsp, %rbp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		andq $-16, %rsp
93		movq %rdi, %rdx
94		movl -4(%rdi), %esi
95		leaq .L._prints_str0(%rip), %rdi
96		# on x86, al represents the number of SIMD registers used as variadic arguments
97		movb $0, %al
98		call printf@plt
99		movq $0, %rdi
100		call fflush@plt
101		movq %rbp, %rsp
102		popq %rbp
103		ret
104	
105	.section .rodata
106	# length of .L._printi_str0
107		.int 2
108	.L._printi_str0:
109		.asciz "%d"
110	.text
111	_printi:
112		pushq %rbp
113		movq %rsp, %rbp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		andq $-16, %rsp
116		movl %edi, %esi
117		leaq .L._printi_str0(%rip), %rdi
118		# on x86, al represents the number of SIMD registers used as variadic arguments
119		movb $0, %al
120		call printf@plt
121		movq $0, %rdi
122		call fflush@plt
123		movq %rbp, %rsp
124		popq %rbp
125		ret
126	
127	.section .rodata
128	# length of .L._println_str0
129		.int 0
130	.L._println_str0:
131		.asciz ""
132	.text
133	_println:
134		pushq %rbp
135		movq %rsp, %rbp
136		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
137		andq $-16, %rsp
138		leaq .L._println_str0(%rip), %rdi
139		call puts@plt
140		movq $0, %rdi
141		call fflush@plt
142		movq %rbp, %rsp
143		popq %rbp
144		ret
145	
146	_exit:
147		pushq %rbp
148		movq %rsp, %rbp
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		andq $-16, %rsp
151		call exit@plt
152		movq %rbp, %rsp
153		popq %rbp
154		ret
155	
156	.section .rodata
157	# length of .L._errOverflow_str0
158		.int 52
159	.L._errOverflow_str0:
160		.asciz "fatal error: integer overflow or underflow occurred\n"
161	.text
162	_errOverflow:
163		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
164		andq $-16, %rsp
165		leaq .L._errOverflow_str0(%rip), %rdi
166		call _prints
167		movb $-1, %dil
168		call exit@plt
===========================================================
-- Finished

