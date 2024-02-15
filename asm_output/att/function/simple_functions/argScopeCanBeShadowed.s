./valid/function/simple_functions/argScopeCanBeShadowed.wacc
calling the reference compiler on ./valid/function/simple_functions/argScopeCanBeShadowed.wacc
-- Test: argScopeCanBeShadowed.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Arguments can be shadowed by the function body

# Output:
# true
#

#Program:

begin
  bool f(int x) is
    bool x = true ;
    return x
  end

  bool x = call f(5);
  println x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
argScopeCanBeShadowed.s contents are:
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
12		movq $5, %rax
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
23		call _printb
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
39		movq $1, %rax
40		movq %rax, %r12
41		movq %r12, %rax
42		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
43		movq %rbp, %rsp
44		popq %r12
45		popq %rbp
46		ret
47		# Stack pointer unchanged, no stack allocated variables
48		# 'ere be dragons: this is 100% dead code, functions always end in returns!
49	
50	.section .rodata
51	# length of .L._printb_str0
52		.int 5
53	.L._printb_str0:
54		.asciz "false"
55	# length of .L._printb_str1
56		.int 4
57	.L._printb_str1:
58		.asciz "true"
59	# length of .L._printb_str2
60		.int 4
61	.L._printb_str2:
62		.asciz "%.*s"
63	.text
64	_printb:
65		pushq %rbp
66		movq %rsp, %rbp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		andq $-16, %rsp
69		cmpb $0, %dil
70		jne .L_printb0
71		leaq .L._printb_str0(%rip), %rdx
72		jmp .L_printb1
73	.L_printb0:
74		leaq .L._printb_str1(%rip), %rdx
75	.L_printb1:
76		movl -4(%rdx), %esi
77		leaq .L._printb_str2(%rip), %rdi
78		# on x86, al represents the number of SIMD registers used as variadic arguments
79		movb $0, %al
80		call printf@plt
81		movq $0, %rdi
82		call fflush@plt
83		movq %rbp, %rsp
84		popq %rbp
85		ret
86	
87	.section .rodata
88	# length of .L._println_str0
89		.int 0
90	.L._println_str0:
91		.asciz ""
92	.text
93	_println:
94		pushq %rbp
95		movq %rsp, %rbp
96		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
97		andq $-16, %rsp
98		leaq .L._println_str0(%rip), %rdi
99		call puts@plt
100		movq $0, %rdi
101		call fflush@plt
102		movq %rbp, %rsp
103		popq %rbp
104		ret
===========================================================
-- Finished

