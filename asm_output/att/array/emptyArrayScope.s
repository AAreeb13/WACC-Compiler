./valid/array/emptyArrayScope.wacc
calling the reference compiler on ./valid/array/emptyArrayScope.wacc
-- Test: emptyArrayScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Scoping shouldn't affect arrays

# Output:

# Program:

begin
  int[] x = [] ;
  begin
    bool y = true
  end
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayScope.s contents are:
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
12		# 0 element array
13		movl $4, %edi
14		call _malloc
15		movq %rax, %r11
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		movq %r11, %r11
18		addq $4, %r11
19		movq $0, %rax
20		movl %eax, -4(%r11)
21		movq %r11, %rax
22		movq %rax, %r12
23		# Stack pointer unchanged, no stack allocated variables
24		movq $1, %rax
25		movq %rax, %r13
26		# Stack pointer unchanged, no stack allocated variables
27		# Stack pointer unchanged, no stack allocated variables
28		movq $0, %rax
29		# popq {%rbx, %r12, %r13}
30		movq (%rsp), %rbx
31		movq 8(%rsp), %r12
32		movq 16(%rsp), %r13
33		addq $24, %rsp
34		popq %rbp
35		ret
36	
37	.section .rodata
38	# length of .L._prints_str0
39		.int 4
40	.L._prints_str0:
41		.asciz "%.*s"
42	.text
43	_prints:
44		pushq %rbp
45		movq %rsp, %rbp
46		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
47		andq $-16, %rsp
48		movq %rdi, %rdx
49		movl -4(%rdi), %esi
50		leaq .L._prints_str0(%rip), %rdi
51		# on x86, al represents the number of SIMD registers used as variadic arguments
52		movb $0, %al
53		call printf@plt
54		movq $0, %rdi
55		call fflush@plt
56		movq %rbp, %rsp
57		popq %rbp
58		ret
59	
60	_malloc:
61		pushq %rbp
62		movq %rsp, %rbp
63		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
64		andq $-16, %rsp
65		call malloc@plt
66		cmpq $0, %rax
67		je _errOutOfMemory
68		movq %rbp, %rsp
69		popq %rbp
70		ret
71	
72	_exit:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		call exit@plt
78		movq %rbp, %rsp
79		popq %rbp
80		ret
81	
82	.section .rodata
83	# length of .L._errOutOfMemory_str0
84		.int 27
85	.L._errOutOfMemory_str0:
86		.asciz "fatal error: out of memory\n"
87	.text
88	_errOutOfMemory:
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		andq $-16, %rsp
91		leaq .L._errOutOfMemory_str0(%rip), %rdi
92		call _prints
93		movb $-1, %dil
94		call exit@plt
===========================================================
-- Finished

