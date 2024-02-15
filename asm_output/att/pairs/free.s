./valid/pairs/free.wacc
calling the reference compiler on ./valid/pairs/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a simple pair

# Output:

# Program:

begin
  pair(int, char) a = newpair(10, 'a') ;
  free a
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
11		movl $16, %edi
12		call _malloc
13		movq %rax, %r11
14		movq $10, %rax
15		movq %rax, (%r11)
16		movq $97, %rax
17		movq %rax, 8(%r11)
18		movq %r11, %rax
19		movq %rax, %r12
20		# Stack pointer unchanged, no stack allocated arguments
21		movq %r12, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _freepair
25		# Stack pointer unchanged, no stack allocated variables
26		movq $0, %rax
27		# popq {%rbx, %r12}
28		movq (%rsp), %rbx
29		movq 8(%rsp), %r12
30		addq $16, %rsp
31		popq %rbp
32		ret
33	
34	_freepair:
35		pushq %rbp
36		movq %rsp, %rbp
37		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
38		andq $-16, %rsp
39		cmpq $0, %rdi
40		je _errNull
41		call free@plt
42		movq %rbp, %rsp
43		popq %rbp
44		ret
45	
46	.section .rodata
47	# length of .L._prints_str0
48		.int 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.text
52	_prints:
53		pushq %rbp
54		movq %rsp, %rbp
55		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
56		andq $-16, %rsp
57		movq %rdi, %rdx
58		movl -4(%rdi), %esi
59		leaq .L._prints_str0(%rip), %rdi
60		# on x86, al represents the number of SIMD registers used as variadic arguments
61		movb $0, %al
62		call printf@plt
63		movq $0, %rdi
64		call fflush@plt
65		movq %rbp, %rsp
66		popq %rbp
67		ret
68	
69	_malloc:
70		pushq %rbp
71		movq %rsp, %rbp
72		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
73		andq $-16, %rsp
74		call malloc@plt
75		cmpq $0, %rax
76		je _errOutOfMemory
77		movq %rbp, %rsp
78		popq %rbp
79		ret
80	
81	_exit:
82		pushq %rbp
83		movq %rsp, %rbp
84		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
85		andq $-16, %rsp
86		call exit@plt
87		movq %rbp, %rsp
88		popq %rbp
89		ret
90	
91	.section .rodata
92	# length of .L._errOutOfMemory_str0
93		.int 27
94	.L._errOutOfMemory_str0:
95		.asciz "fatal error: out of memory\n"
96	.text
97	_errOutOfMemory:
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		andq $-16, %rsp
100		leaq .L._errOutOfMemory_str0(%rip), %rdi
101		call _prints
102		movb $-1, %dil
103		call exit@plt
104	
105	.section .rodata
106	# length of .L._errNull_str0
107		.int 45
108	.L._errNull_str0:
109		.asciz "fatal error: null pair dereferenced or freed\n"
110	.text
111	_errNull:
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		andq $-16, %rsp
114		leaq .L._errNull_str0(%rip), %rdi
115		call _prints
116		movb $-1, %dil
117		call exit@plt
===========================================================
-- Finished

