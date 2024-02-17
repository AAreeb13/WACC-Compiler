./valid/function/simple_functions/functionSimpleLoop.wacc
calling the reference compiler on ./valid/function/simple_functions/functionSimpleLoop.wacc
-- Test: functionSimpleLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# define and call a function with a simple loop

# Output:
# 10
#

# Program:

begin
  int f(int n) is
    int i = 0;
    while i < n do
      i = i + 1
    done;
    return i
  end
  int x = call f(10) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionSimpleLoop.s contents are:
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
11		# Stack pointer unchanged, no stack allocated arguments
12		movq $10, %rax
13		movq %rax, %rdi
14		call wacc_f
15		movq %rax, %r11
16		# Stack pointer unchanged, no stack allocated arguments
17		movq %r11, %rax
18		movq %rax, %r12
19		# Stack pointer unchanged, no stack allocated arguments
20		movq %r12, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _printi
24		call _println
25		# Stack pointer unchanged, no stack allocated variables
26		movq $0, %rax
27		# popq {%rbx, %r12}
28		movq (%rsp), %rbx
29		movq 8(%rsp), %r12
30		addq $16, %rsp
31		popq %rbp
32		ret
33	
34	wacc_f:
35		pushq %rbp
36		pushq %r12
37		movq %rsp, %rbp
38		# Stack pointer unchanged, no stack allocated variables
39		movq $0, %rax
40		movq %rax, %r12
41		jmp .L0
42	.L1:
43		movl %r12d, %eax
44		addl $1, %eax
45		jo _errOverflow
46		movslq %eax, %rax
47		pushq %rax
48		popq %rax
49		movq %rax, %rax
50		movq %rax, %r12
51	.L0:
52		cmpq %rdi, %r12
53		jl .L1
54		movq %r12, %rax
55		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
56		movq %rbp, %rsp
57		popq %r12
58		popq %rbp
59		ret
60		# Stack pointer unchanged, no stack allocated variables
61		# 'ere be dragons: this is 100% dead code, functions always end in returns!
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
86	.section .rodata
87	# length of .L._printi_str0
88		.int 2
89	.L._printi_str0:
90		.asciz "%d"
91	.text
92	_printi:
93		pushq %rbp
94		movq %rsp, %rbp
95		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
96		andq $-16, %rsp
97		movl %edi, %esi
98		leaq .L._printi_str0(%rip), %rdi
99		# on x86, al represents the number of SIMD registers used as variadic arguments
100		movb $0, %al
101		call printf@plt
102		movq $0, %rdi
103		call fflush@plt
104		movq %rbp, %rsp
105		popq %rbp
106		ret
107	
108	.section .rodata
109	# length of .L._println_str0
110		.int 0
111	.L._println_str0:
112		.asciz ""
113	.text
114	_println:
115		pushq %rbp
116		movq %rsp, %rbp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		andq $-16, %rsp
119		leaq .L._println_str0(%rip), %rdi
120		call puts@plt
121		movq $0, %rdi
122		call fflush@plt
123		movq %rbp, %rsp
124		popq %rbp
125		ret
126	
127	_exit:
128		pushq %rbp
129		movq %rsp, %rbp
130		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
131		andq $-16, %rsp
132		call exit@plt
133		movq %rbp, %rsp
134		popq %rbp
135		ret
136	
137	.section .rodata
138	# length of .L._errOverflow_str0
139		.int 52
140	.L._errOverflow_str0:
141		.asciz "fatal error: integer overflow or underflow occurred\n"
142	.text
143	_errOverflow:
144		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
145		andq $-16, %rsp
146		leaq .L._errOverflow_str0(%rip), %rdi
147		call _prints
148		movb $-1, %dil
149		call exit@plt
===========================================================
-- Finished

