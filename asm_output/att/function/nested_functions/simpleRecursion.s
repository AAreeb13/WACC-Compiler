./valid/function/nested_functions/simpleRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/simpleRecursion.wacc
-- Test: simpleRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple recursive function

# Output:

# Program:

begin
  int rec(int x) is
    if x == 0
    then
      skip
    else
      int y = call rec(x - 1)
    fi ;
    return 42
  end

  int x = 0 ;
  x = call rec(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
simpleRecursion.s contents are:
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
14		movq $8, %rax
15		movq %rax, %rdi
16		call wacc_rec
17		movq %rax, %r11
18		# Stack pointer unchanged, no stack allocated arguments
19		movq %r11, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated variables
22		movq $0, %rax
23		# popq {%rbx, %r12}
24		movq (%rsp), %rbx
25		movq 8(%rsp), %r12
26		addq $16, %rsp
27		popq %rbp
28		ret
29	
30	wacc_rec:
31		pushq %rbp
32		pushq %r12
33		movq %rsp, %rbp
34		cmpq $0, %rdi
35		je .L0
36		# Stack pointer unchanged, no stack allocated variables
37		pushq %rdi
38		# Set up R11 as a temporary second base pointer for the caller saved things
39		movq %rsp, %r11
40		# Stack pointer unchanged, no stack allocated arguments
41		movl %edi, %eax
42		subl $1, %eax
43		jo _errOverflow
44		movslq %eax, %rax
45		pushq %rax
46		popq %rax
47		movq %rax, %rax
48		movq %rax, %rdi
49		call wacc_rec
50		movq %rax, %r11
51		# Stack pointer unchanged, no stack allocated arguments
52		popq %rdi
53		movq %r11, %rax
54		movq %rax, %r12
55		# Stack pointer unchanged, no stack allocated variables
56		jmp .L1
57	.L0:
58	.L1:
59		movq $42, %rax
60		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
61		movq %rbp, %rsp
62		popq %r12
63		popq %rbp
64		ret
65		# 'ere be dragons: this is 100% dead code, functions always end in returns!
66	
67	.section .rodata
68	# length of .L._prints_str0
69		.int 4
70	.L._prints_str0:
71		.asciz "%.*s"
72	.text
73	_prints:
74		pushq %rbp
75		movq %rsp, %rbp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		andq $-16, %rsp
78		movq %rdi, %rdx
79		movl -4(%rdi), %esi
80		leaq .L._prints_str0(%rip), %rdi
81		# on x86, al represents the number of SIMD registers used as variadic arguments
82		movb $0, %al
83		call printf@plt
84		movq $0, %rdi
85		call fflush@plt
86		movq %rbp, %rsp
87		popq %rbp
88		ret
89	
90	_exit:
91		pushq %rbp
92		movq %rsp, %rbp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		andq $-16, %rsp
95		call exit@plt
96		movq %rbp, %rsp
97		popq %rbp
98		ret
99	
100	.section .rodata
101	# length of .L._errOverflow_str0
102		.int 52
103	.L._errOverflow_str0:
104		.asciz "fatal error: integer overflow or underflow occurred\n"
105	.text
106	_errOverflow:
107		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
108		andq $-16, %rsp
109		leaq .L._errOverflow_str0(%rip), %rdi
110		call _prints
111		movb $-1, %dil
112		call exit@plt
===========================================================
-- Finished

