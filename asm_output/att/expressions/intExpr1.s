./valid/expressions/intExpr1.wacc
calling the reference compiler on ./valid/expressions/intExpr1.wacc
-- Test: intExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex integer expression

# Output:
# Correct
#

# Program:

begin
  int a = ( 10 * 1 + 2 * 15 );
  if a == 40 then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intExpr1.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 5
4	.L.str0:
5		.asciz "Wrong"
6	# length of .L.str1
7		.int 7
8	.L.str1:
9		.asciz "Correct"
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $10, %rax
20		imull $1, %eax, %eax
21		jo _errOverflow
22		movslq %eax, %rax
23		pushq %rax
24		movq $2, %rax
25		imull $15, %eax, %eax
26		jo _errOverflow
27		movslq %eax, %rax
28		pushq %rax
29		popq %rbx
30		popq %rax
31		movl %eax, %eax
32		addl %ebx, %eax
33		jo _errOverflow
34		movslq %eax, %rax
35		pushq %rax
36		popq %rax
37		movq %rax, %rax
38		movq %rax, %r12
39		cmpq $40, %r12
40		je .L0
41		# Stack pointer unchanged, no stack allocated arguments
42		leaq .L.str0(%rip), %rax
43		pushq %rax
44		popq %rax
45		movq %rax, %rax
46		movq %rax, %rdi
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _prints
49		call _println
50		jmp .L1
51	.L0:
52		# Stack pointer unchanged, no stack allocated arguments
53		leaq .L.str1(%rip), %rax
54		pushq %rax
55		popq %rax
56		movq %rax, %rax
57		movq %rax, %rdi
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _prints
60		call _println
61	.L1:
62		# Stack pointer unchanged, no stack allocated variables
63		movq $0, %rax
64		# popq {%rbx, %r12}
65		movq (%rsp), %rbx
66		movq 8(%rsp), %r12
67		addq $16, %rsp
68		popq %rbp
69		ret
70	
71	.section .rodata
72	# length of .L._prints_str0
73		.int 4
74	.L._prints_str0:
75		.asciz "%.*s"
76	.text
77	_prints:
78		pushq %rbp
79		movq %rsp, %rbp
80		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
81		andq $-16, %rsp
82		movq %rdi, %rdx
83		movl -4(%rdi), %esi
84		leaq .L._prints_str0(%rip), %rdi
85		# on x86, al represents the number of SIMD registers used as variadic arguments
86		movb $0, %al
87		call printf@plt
88		movq $0, %rdi
89		call fflush@plt
90		movq %rbp, %rsp
91		popq %rbp
92		ret
93	
94	.section .rodata
95	# length of .L._println_str0
96		.int 0
97	.L._println_str0:
98		.asciz ""
99	.text
100	_println:
101		pushq %rbp
102		movq %rsp, %rbp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		andq $-16, %rsp
105		leaq .L._println_str0(%rip), %rdi
106		call puts@plt
107		movq $0, %rdi
108		call fflush@plt
109		movq %rbp, %rsp
110		popq %rbp
111		ret
112	
113	_exit:
114		pushq %rbp
115		movq %rsp, %rbp
116		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
117		andq $-16, %rsp
118		call exit@plt
119		movq %rbp, %rsp
120		popq %rbp
121		ret
122	
123	.section .rodata
124	# length of .L._errOverflow_str0
125		.int 52
126	.L._errOverflow_str0:
127		.asciz "fatal error: integer overflow or underflow occurred\n"
128	.text
129	_errOverflow:
130		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
131		andq $-16, %rsp
132		leaq .L._errOverflow_str0(%rip), %rdi
133		call _prints
134		movb $-1, %dil
135		call exit@plt
===========================================================
-- Finished

