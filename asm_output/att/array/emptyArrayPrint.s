./valid/array/emptyArrayPrint.wacc
calling the reference compiler on ./valid/array/emptyArrayPrint.wacc
-- Test: emptyArrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and print after

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayPrint.s contents are:
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
11		# 0 element array
12		movl $4, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $0, %rax
19		movl %eax, -4(%r11)
20		movq %r11, %rax
21		movq %rax, %r12
22		# Stack pointer unchanged, no stack allocated arguments
23		movq $1, %rax
24		movq %rax, %rdi
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printb
27		call _println
28		# Stack pointer unchanged, no stack allocated variables
29		movq $0, %rax
30		# popq {%rbx, %r12}
31		movq (%rsp), %rbx
32		movq 8(%rsp), %r12
33		addq $16, %rsp
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
72	.section .rodata
73	# length of .L._printb_str0
74		.int 5
75	.L._printb_str0:
76		.asciz "false"
77	# length of .L._printb_str1
78		.int 4
79	.L._printb_str1:
80		.asciz "true"
81	# length of .L._printb_str2
82		.int 4
83	.L._printb_str2:
84		.asciz "%.*s"
85	.text
86	_printb:
87		pushq %rbp
88		movq %rsp, %rbp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		andq $-16, %rsp
91		cmpb $0, %dil
92		jne .L_printb0
93		leaq .L._printb_str0(%rip), %rdx
94		jmp .L_printb1
95	.L_printb0:
96		leaq .L._printb_str1(%rip), %rdx
97	.L_printb1:
98		movl -4(%rdx), %esi
99		leaq .L._printb_str2(%rip), %rdi
100		# on x86, al represents the number of SIMD registers used as variadic arguments
101		movb $0, %al
102		call printf@plt
103		movq $0, %rdi
104		call fflush@plt
105		movq %rbp, %rsp
106		popq %rbp
107		ret
108	
109	.section .rodata
110	# length of .L._println_str0
111		.int 0
112	.L._println_str0:
113		.asciz ""
114	.text
115	_println:
116		pushq %rbp
117		movq %rsp, %rbp
118		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
119		andq $-16, %rsp
120		leaq .L._println_str0(%rip), %rdi
121		call puts@plt
122		movq $0, %rdi
123		call fflush@plt
124		movq %rbp, %rsp
125		popq %rbp
126		ret
127	
128	_exit:
129		pushq %rbp
130		movq %rsp, %rbp
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		andq $-16, %rsp
133		call exit@plt
134		movq %rbp, %rsp
135		popq %rbp
136		ret
137	
138	.section .rodata
139	# length of .L._errOutOfMemory_str0
140		.int 27
141	.L._errOutOfMemory_str0:
142		.asciz "fatal error: out of memory\n"
143	.text
144	_errOutOfMemory:
145		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
146		andq $-16, %rsp
147		leaq .L._errOutOfMemory_str0(%rip), %rdi
148		call _prints
149		movb $-1, %dil
150		call exit@plt
===========================================================
-- Finished

