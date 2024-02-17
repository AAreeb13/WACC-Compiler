./valid/expressions/boolExpr1.wacc
calling the reference compiler on ./valid/expressions/boolExpr1.wacc
-- Test: boolExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex boolean expression

# Output:
# Correct
#

# Program:

begin
  bool b = ! ( ( true && false) || (true && false) );
  if b == true then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolExpr1.s contents are:
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
19		movq $1, %rax
20		cmpq $1, %rax
21		jne .L0
22		movq $0, %rax
23		cmpq $1, %rax
24	.L0:
25		sete %al
26		movsbq %al, %rax
27		pushq %rax
28		popq %rax
29		cmpq $1, %rax
30		je .L1
31		movq $1, %rax
32		cmpq $1, %rax
33		jne .L2
34		movq $0, %rax
35		cmpq $1, %rax
36	.L2:
37		sete %al
38		movsbq %al, %rax
39		pushq %rax
40		popq %rax
41		cmpq $1, %rax
42	.L1:
43		setne %al
44		movsbq %al, %rax
45		pushq %rax
46		popq %rax
47		movq %rax, %rax
48		movq %rax, %r12
49		cmpq $1, %r12
50		je .L3
51		# Stack pointer unchanged, no stack allocated arguments
52		leaq .L.str0(%rip), %rax
53		pushq %rax
54		popq %rax
55		movq %rax, %rax
56		movq %rax, %rdi
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _prints
59		call _println
60		jmp .L4
61	.L3:
62		# Stack pointer unchanged, no stack allocated arguments
63		leaq .L.str1(%rip), %rax
64		pushq %rax
65		popq %rax
66		movq %rax, %rax
67		movq %rax, %rdi
68		# statement primitives do not return results (but will clobber r0/rax)
69		call _prints
70		call _println
71	.L4:
72		# Stack pointer unchanged, no stack allocated variables
73		movq $0, %rax
74		# popq {%rbx, %r12}
75		movq (%rsp), %rbx
76		movq 8(%rsp), %r12
77		addq $16, %rsp
78		popq %rbp
79		ret
80	
81	.section .rodata
82	# length of .L._prints_str0
83		.int 4
84	.L._prints_str0:
85		.asciz "%.*s"
86	.text
87	_prints:
88		pushq %rbp
89		movq %rsp, %rbp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		andq $-16, %rsp
92		movq %rdi, %rdx
93		movl -4(%rdi), %esi
94		leaq .L._prints_str0(%rip), %rdi
95		# on x86, al represents the number of SIMD registers used as variadic arguments
96		movb $0, %al
97		call printf@plt
98		movq $0, %rdi
99		call fflush@plt
100		movq %rbp, %rsp
101		popq %rbp
102		ret
103	
104	.section .rodata
105	# length of .L._println_str0
106		.int 0
107	.L._println_str0:
108		.asciz ""
109	.text
110	_println:
111		pushq %rbp
112		movq %rsp, %rbp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		andq $-16, %rsp
115		leaq .L._println_str0(%rip), %rdi
116		call puts@plt
117		movq $0, %rdi
118		call fflush@plt
119		movq %rbp, %rsp
120		popq %rbp
121		ret
===========================================================
-- Finished

