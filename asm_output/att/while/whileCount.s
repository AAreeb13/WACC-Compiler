./valid/while/whileCount.wacc
calling the reference compiler on ./valid/while/whileCount.wacc
-- Test: whileCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple counting while loop

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  while i <= 10 do
    println i ;
    i = i + 1
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileCount.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 20
4	.L.str0:
5		.asciz "Can you count to 10?"
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		movq $1, %rax
16		movq %rax, %r12
17		# Stack pointer unchanged, no stack allocated arguments
18		leaq .L.str0(%rip), %rax
19		pushq %rax
20		popq %rax
21		movq %rax, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _prints
25		call _println
26		jmp .L0
27	.L1:
28		# Stack pointer unchanged, no stack allocated arguments
29		movq %r12, %rax
30		movq %rax, %rdi
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printi
33		call _println
34		movl %r12d, %eax
35		addl $1, %eax
36		jo _errOverflow
37		movslq %eax, %rax
38		pushq %rax
39		popq %rax
40		movq %rax, %rax
41		movq %rax, %r12
42	.L0:
43		cmpq $10, %r12
44		jle .L1
45		# Stack pointer unchanged, no stack allocated variables
46		movq $0, %rax
47		# popq {%rbx, %r12}
48		movq (%rsp), %rbx
49		movq 8(%rsp), %r12
50		addq $16, %rsp
51		popq %rbp
52		ret
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

