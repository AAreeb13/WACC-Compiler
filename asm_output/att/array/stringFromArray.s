./valid/array/stringFromArray.wacc
calling the reference compiler on ./valid/array/stringFromArray.wacc
-- Test: stringFromArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# char[] can be directly assigned to string

# Output:

# Program:

begin
  string a = ['a', 'b'] ;
  exit 0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringFromArray.s contents are:
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
11		# 2 element array
12		movl $6, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $2, %rax
19		movl %eax, -4(%r11)
20		movq $97, %rax
21		movb %al, (%r11)
22		movq $98, %rax
23		movb %al, 1(%r11)
24		movq %r11, %rax
25		movq %rax, %r12
26		# Stack pointer unchanged, no stack allocated arguments
27		movq $0, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _exit
31		# Stack pointer unchanged, no stack allocated variables
32		movq $0, %rax
33		# popq {%rbx, %r12}
34		movq (%rsp), %rbx
35		movq 8(%rsp), %r12
36		addq $16, %rsp
37		popq %rbp
38		ret
39	
40	.section .rodata
41	# length of .L._prints_str0
42		.int 4
43	.L._prints_str0:
44		.asciz "%.*s"
45	.text
46	_prints:
47		pushq %rbp
48		movq %rsp, %rbp
49		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
50		andq $-16, %rsp
51		movq %rdi, %rdx
52		movl -4(%rdi), %esi
53		leaq .L._prints_str0(%rip), %rdi
54		# on x86, al represents the number of SIMD registers used as variadic arguments
55		movb $0, %al
56		call printf@plt
57		movq $0, %rdi
58		call fflush@plt
59		movq %rbp, %rsp
60		popq %rbp
61		ret
62	
63	_malloc:
64		pushq %rbp
65		movq %rsp, %rbp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		andq $-16, %rsp
68		call malloc@plt
69		cmpq $0, %rax
70		je _errOutOfMemory
71		movq %rbp, %rsp
72		popq %rbp
73		ret
74	
75	_exit:
76		pushq %rbp
77		movq %rsp, %rbp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		andq $-16, %rsp
80		call exit@plt
81		movq %rbp, %rsp
82		popq %rbp
83		ret
84	
85	.section .rodata
86	# length of .L._errOutOfMemory_str0
87		.int 27
88	.L._errOutOfMemory_str0:
89		.asciz "fatal error: out of memory\n"
90	.text
91	_errOutOfMemory:
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		leaq .L._errOutOfMemory_str0(%rip), %rdi
95		call _prints
96		movb $-1, %dil
97		call exit@plt
===========================================================
-- Finished

