./valid/function/simple_functions/punning.wacc
calling the reference compiler on ./valid/function/simple_functions/punning.wacc
-- Test: punning.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Functions should be able to have the same name as variables
# Thanks to Jordan Hall

# Output:
# 0
#

begin
    int inc(int x) is
        return x + 1
    end
    int inc = 0;
    int tmp = call inc(inc);
    println inc
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
punning.s contents are:
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
12		movq $0, %rax
13		movq %rax, %r12
14		# Stack pointer unchanged, no stack allocated arguments
15		movq %r12, %rax
16		movq %rax, %rdi
17		call wacc_inc
18		movq %rax, %r11
19		# Stack pointer unchanged, no stack allocated arguments
20		movq %r11, %rax
21		movq %rax, %r13
22		# Stack pointer unchanged, no stack allocated arguments
23		movq %r12, %rax
24		movq %rax, %rdi
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printi
27		call _println
28		# Stack pointer unchanged, no stack allocated variables
29		movq $0, %rax
30		# popq {%rbx, %r12, %r13}
31		movq (%rsp), %rbx
32		movq 8(%rsp), %r12
33		movq 16(%rsp), %r13
34		addq $24, %rsp
35		popq %rbp
36		ret
37	
38	wacc_inc:
39		pushq %rbp
40		movq %rsp, %rbp
41		movl %edi, %eax
42		addl $1, %eax
43		jo _errOverflow
44		movslq %eax, %rax
45		pushq %rax
46		popq %rax
47		movq %rax, %rax
48		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
49		movq %rbp, %rsp
50		popq %rbp
51		ret
52		# 'ere be dragons: this is 100% dead code, functions always end in returns!
53	
54	.section .rodata
55	# length of .L._prints_str0
56		.int 4
57	.L._prints_str0:
58		.asciz "%.*s"
59	.text
60	_prints:
61		pushq %rbp
62		movq %rsp, %rbp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		andq $-16, %rsp
65		movq %rdi, %rdx
66		movl -4(%rdi), %esi
67		leaq .L._prints_str0(%rip), %rdi
68		# on x86, al represents the number of SIMD registers used as variadic arguments
69		movb $0, %al
70		call printf@plt
71		movq $0, %rdi
72		call fflush@plt
73		movq %rbp, %rsp
74		popq %rbp
75		ret
76	
77	.section .rodata
78	# length of .L._printi_str0
79		.int 2
80	.L._printi_str0:
81		.asciz "%d"
82	.text
83	_printi:
84		pushq %rbp
85		movq %rsp, %rbp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		andq $-16, %rsp
88		movl %edi, %esi
89		leaq .L._printi_str0(%rip), %rdi
90		# on x86, al represents the number of SIMD registers used as variadic arguments
91		movb $0, %al
92		call printf@plt
93		movq $0, %rdi
94		call fflush@plt
95		movq %rbp, %rsp
96		popq %rbp
97		ret
98	
99	.section .rodata
100	# length of .L._println_str0
101		.int 0
102	.L._println_str0:
103		.asciz ""
104	.text
105	_println:
106		pushq %rbp
107		movq %rsp, %rbp
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		andq $-16, %rsp
110		leaq .L._println_str0(%rip), %rdi
111		call puts@plt
112		movq $0, %rdi
113		call fflush@plt
114		movq %rbp, %rsp
115		popq %rbp
116		ret
117	
118	_exit:
119		pushq %rbp
120		movq %rsp, %rbp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		andq $-16, %rsp
123		call exit@plt
124		movq %rbp, %rsp
125		popq %rbp
126		ret
127	
128	.section .rodata
129	# length of .L._errOverflow_str0
130		.int 52
131	.L._errOverflow_str0:
132		.asciz "fatal error: integer overflow or underflow occurred\n"
133	.text
134	_errOverflow:
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		andq $-16, %rsp
137		leaq .L._errOverflow_str0(%rip), %rdi
138		call _prints
139		movb $-1, %dil
140		call exit@plt
===========================================================
-- Finished

