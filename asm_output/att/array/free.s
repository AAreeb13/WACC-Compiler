./valid/array/free.wacc
calling the reference compiler on ./valid/array/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free an array

# Output:

# Program:

begin
  int[] arr = [1, 2, 3] ;
  free arr
end


---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
11		# 3 element array
12		movl $16, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $3, %rax
19		movl %eax, -4(%r11)
20		movq $1, %rax
21		movl %eax, (%r11)
22		movq $2, %rax
23		movl %eax, 4(%r11)
24		movq $3, %rax
25		movl %eax, 8(%r11)
26		movq %r11, %rax
27		movq %rax, %r12
28		# Stack pointer unchanged, no stack allocated arguments
29		# array pointers are shifted forward by 4 bytes, so correct it back to original pointer before free
30		movq %r12, %rax
31		subq $4, %rax
32		pushq %rax
33		popq %rax
34		movq %rax, %rax
35		movq %rax, %rdi
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _free
38		# Stack pointer unchanged, no stack allocated variables
39		movq $0, %rax
40		# popq {%rbx, %r12}
41		movq (%rsp), %rbx
42		movq 8(%rsp), %r12
43		addq $16, %rsp
44		popq %rbp
45		ret
46	
47	.section .rodata
48	# length of .L._prints_str0
49		.int 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.text
53	_prints:
54		pushq %rbp
55		movq %rsp, %rbp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		andq $-16, %rsp
58		movq %rdi, %rdx
59		movl -4(%rdi), %esi
60		leaq .L._prints_str0(%rip), %rdi
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		movb $0, %al
63		call printf@plt
64		movq $0, %rdi
65		call fflush@plt
66		movq %rbp, %rsp
67		popq %rbp
68		ret
69	
70	_malloc:
71		pushq %rbp
72		movq %rsp, %rbp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		andq $-16, %rsp
75		call malloc@plt
76		cmpq $0, %rax
77		je _errOutOfMemory
78		movq %rbp, %rsp
79		popq %rbp
80		ret
81	
82	_free:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		call free@plt
88		movq %rbp, %rsp
89		popq %rbp
90		ret
91	
92	_exit:
93		pushq %rbp
94		movq %rsp, %rbp
95		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
96		andq $-16, %rsp
97		call exit@plt
98		movq %rbp, %rsp
99		popq %rbp
100		ret
101	
102	.section .rodata
103	# length of .L._errOutOfMemory_str0
104		.int 27
105	.L._errOutOfMemory_str0:
106		.asciz "fatal error: out of memory\n"
107	.text
108	_errOutOfMemory:
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		andq $-16, %rsp
111		leaq .L._errOutOfMemory_str0(%rip), %rdi
112		call _prints
113		movb $-1, %dil
114		call exit@plt
===========================================================
-- Finished

