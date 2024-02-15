./valid/function/simple_functions/negFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/negFunction.wacc
-- Test: negFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple negation function definition and usage

# Output:
# true
# false
# true
#

# Program:

begin
  bool neg(bool b) is
    return !b
  end
  bool b = true ;
  println b ;
  b = call neg(b) ;
  println b ;
  b = call neg(b) ;
  b = call neg(b) ;
  b = call neg(b) ;
  println b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negFunction.s contents are:
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
11		movq $1, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printb
18		call _println
19		# Stack pointer unchanged, no stack allocated arguments
20		movq %r12, %rax
21		movq %rax, %rdi
22		call wacc_neg
23		movq %rax, %r11
24		# Stack pointer unchanged, no stack allocated arguments
25		movq %r11, %rax
26		movq %rax, %r12
27		# Stack pointer unchanged, no stack allocated arguments
28		movq %r12, %rax
29		movq %rax, %rdi
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printb
32		call _println
33		# Stack pointer unchanged, no stack allocated arguments
34		movq %r12, %rax
35		movq %rax, %rdi
36		call wacc_neg
37		movq %rax, %r11
38		# Stack pointer unchanged, no stack allocated arguments
39		movq %r11, %rax
40		movq %rax, %r12
41		# Stack pointer unchanged, no stack allocated arguments
42		movq %r12, %rax
43		movq %rax, %rdi
44		call wacc_neg
45		movq %rax, %r11
46		# Stack pointer unchanged, no stack allocated arguments
47		movq %r11, %rax
48		movq %rax, %r12
49		# Stack pointer unchanged, no stack allocated arguments
50		movq %r12, %rax
51		movq %rax, %rdi
52		call wacc_neg
53		movq %rax, %r11
54		# Stack pointer unchanged, no stack allocated arguments
55		movq %r11, %rax
56		movq %rax, %r12
57		# Stack pointer unchanged, no stack allocated arguments
58		movq %r12, %rax
59		movq %rax, %rdi
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _printb
62		call _println
63		# Stack pointer unchanged, no stack allocated variables
64		movq $0, %rax
65		# popq {%rbx, %r12}
66		movq (%rsp), %rbx
67		movq 8(%rsp), %r12
68		addq $16, %rsp
69		popq %rbp
70		ret
71	
72	wacc_neg:
73		pushq %rbp
74		movq %rsp, %rbp
75		cmpq $1, %rdi
76		setne %al
77		movsbq %al, %rax
78		pushq %rax
79		popq %rax
80		movq %rax, %rax
81		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
82		movq %rbp, %rsp
83		popq %rbp
84		ret
85		# 'ere be dragons: this is 100% dead code, functions always end in returns!
86	
87	.section .rodata
88	# length of .L._printb_str0
89		.int 5
90	.L._printb_str0:
91		.asciz "false"
92	# length of .L._printb_str1
93		.int 4
94	.L._printb_str1:
95		.asciz "true"
96	# length of .L._printb_str2
97		.int 4
98	.L._printb_str2:
99		.asciz "%.*s"
100	.text
101	_printb:
102		pushq %rbp
103		movq %rsp, %rbp
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		andq $-16, %rsp
106		cmpb $0, %dil
107		jne .L_printb0
108		leaq .L._printb_str0(%rip), %rdx
109		jmp .L_printb1
110	.L_printb0:
111		leaq .L._printb_str1(%rip), %rdx
112	.L_printb1:
113		movl -4(%rdx), %esi
114		leaq .L._printb_str2(%rip), %rdi
115		# on x86, al represents the number of SIMD registers used as variadic arguments
116		movb $0, %al
117		call printf@plt
118		movq $0, %rdi
119		call fflush@plt
120		movq %rbp, %rsp
121		popq %rbp
122		ret
123	
124	.section .rodata
125	# length of .L._println_str0
126		.int 0
127	.L._println_str0:
128		.asciz ""
129	.text
130	_println:
131		pushq %rbp
132		movq %rsp, %rbp
133		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
134		andq $-16, %rsp
135		leaq .L._println_str0(%rip), %rdi
136		call puts@plt
137		movq $0, %rdi
138		call fflush@plt
139		movq %rbp, %rsp
140		popq %rbp
141		ret
===========================================================
-- Finished

