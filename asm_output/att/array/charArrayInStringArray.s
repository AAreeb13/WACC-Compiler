./valid/array/charArrayInStringArray.wacc
calling the reference compiler on ./valid/array/charArrayInStringArray.wacc
-- Test: charArrayInStringArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a char[] can fit into a string[]
# Thanks to Iurii Zamiatin for finding this

# Output:

# Program:

begin
    char[] testArr = ['a', 'b', 'c'];
    string[] arr = [testArr, "box", "foo", "bar"]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charArrayInStringArray.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 3
4	.L.str0:
5		.asciz "box"
6	# length of .L.str1
7		.int 3
8	.L.str1:
9		.asciz "foo"
10	# length of .L.str2
11		.int 3
12	.L.str2:
13		.asciz "bar"
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12, %r13}
18		subq $24, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %rsp, %rbp
23		# Stack pointer unchanged, no stack allocated variables
24		# 3 element array
25		movl $7, %edi
26		call _malloc
27		movq %rax, %r11
28		# array pointers are shifted forwards by 4 bytes (to account for size)
29		movq %r11, %r11
30		addq $4, %r11
31		movq $3, %rax
32		movl %eax, -4(%r11)
33		movq $97, %rax
34		movb %al, (%r11)
35		movq $98, %rax
36		movb %al, 1(%r11)
37		movq $99, %rax
38		movb %al, 2(%r11)
39		movq %r11, %rax
40		movq %rax, %r12
41		# 4 element array
42		movl $36, %edi
43		call _malloc
44		movq %rax, %r11
45		# array pointers are shifted forwards by 4 bytes (to account for size)
46		movq %r11, %r11
47		addq $4, %r11
48		movq $4, %rax
49		movl %eax, -4(%r11)
50		movq %r12, %rax
51		movq %rax, (%r11)
52		leaq .L.str0(%rip), %rax
53		pushq %rax
54		popq %rax
55		movq %rax, %rax
56		movq %rax, 8(%r11)
57		leaq .L.str1(%rip), %rax
58		pushq %rax
59		popq %rax
60		movq %rax, %rax
61		movq %rax, 16(%r11)
62		leaq .L.str2(%rip), %rax
63		pushq %rax
64		popq %rax
65		movq %rax, %rax
66		movq %rax, 24(%r11)
67		movq %r11, %rax
68		movq %rax, %r13
69		# Stack pointer unchanged, no stack allocated variables
70		movq $0, %rax
71		# popq {%rbx, %r12, %r13}
72		movq (%rsp), %rbx
73		movq 8(%rsp), %r12
74		movq 16(%rsp), %r13
75		addq $24, %rsp
76		popq %rbp
77		ret
78	
79	.section .rodata
80	# length of .L._prints_str0
81		.int 4
82	.L._prints_str0:
83		.asciz "%.*s"
84	.text
85	_prints:
86		pushq %rbp
87		movq %rsp, %rbp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		andq $-16, %rsp
90		movq %rdi, %rdx
91		movl -4(%rdi), %esi
92		leaq .L._prints_str0(%rip), %rdi
93		# on x86, al represents the number of SIMD registers used as variadic arguments
94		movb $0, %al
95		call printf@plt
96		movq $0, %rdi
97		call fflush@plt
98		movq %rbp, %rsp
99		popq %rbp
100		ret
101	
102	_malloc:
103		pushq %rbp
104		movq %rsp, %rbp
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		andq $-16, %rsp
107		call malloc@plt
108		cmpq $0, %rax
109		je _errOutOfMemory
110		movq %rbp, %rsp
111		popq %rbp
112		ret
113	
114	_exit:
115		pushq %rbp
116		movq %rsp, %rbp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		andq $-16, %rsp
119		call exit@plt
120		movq %rbp, %rsp
121		popq %rbp
122		ret
123	
124	.section .rodata
125	# length of .L._errOutOfMemory_str0
126		.int 27
127	.L._errOutOfMemory_str0:
128		.asciz "fatal error: out of memory\n"
129	.text
130	_errOutOfMemory:
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		andq $-16, %rsp
133		leaq .L._errOutOfMemory_str0(%rip), %rdi
134		call _prints
135		movb $-1, %dil
136		call exit@plt
===========================================================
-- Finished

