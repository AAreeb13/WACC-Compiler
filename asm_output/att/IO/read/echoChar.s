./valid/IO/read/echoChar.wacc
calling the reference compiler on ./valid/IO/read/echoChar.wacc
-- Test: echoChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# echo the user's input char

# Input: K

# Output:
# enter a character to echo
# K
#

# Program:

begin
	char c = '\0' ;
  println "enter a character to echo";
	read c ;
	println c
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
echoChar.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 25
4	.L.str0:
5		.asciz "enter a character to echo"
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		movq $0, %rax
16		movq %rax, %r12
17		# Stack pointer unchanged, no stack allocated arguments
18		leaq .L.str0(%rip), %rax
19		pushq %rax
20		popq %rax
21		movq %rax, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _prints
25		call _println
26		# Stack pointer unchanged, no stack allocated arguments
27		# load the current value in the destination of the read so it supports defaults
28		movq %r12, %rax
29		movq %rax, %rdi
30		call _readc
31		movq %rax, %r11
32		movq %r11, %rax
33		movq %rax, %r12
34		# Stack pointer unchanged, no stack allocated arguments
35		movq %r12, %rax
36		movq %rax, %rdi
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _printc
39		call _println
40		# Stack pointer unchanged, no stack allocated variables
41		movq $0, %rax
42		# popq {%rbx, %r12}
43		movq (%rsp), %rbx
44		movq 8(%rsp), %r12
45		addq $16, %rsp
46		popq %rbp
47		ret
48	
49	.section .rodata
50	# length of .L._readc_str0
51		.int 3
52	.L._readc_str0:
53		.asciz " %c"
54	.text
55	_readc:
56		pushq %rbp
57		movq %rsp, %rbp
58		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
59		andq $-16, %rsp
60		# RDI contains the "original" value of the destination of the read
61		# allocate space on the stack to store the read: preserve alignment!
62		# the passed default argument should be stored in case of EOF
63		subq $16, %rsp
64		movb %dil, (%rsp)
65		leaq (%rsp), %rsi
66		leaq .L._readc_str0(%rip), %rdi
67		# on x86, al represents the number of SIMD registers used as variadic arguments
68		movb $0, %al
69		call scanf@plt
70		movsbq (%rsp), %rax
71		addq $16, %rsp
72		movq %rbp, %rsp
73		popq %rbp
74		ret
75	
76	.section .rodata
77	# length of .L._prints_str0
78		.int 4
79	.L._prints_str0:
80		.asciz "%.*s"
81	.text
82	_prints:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		movq %rdi, %rdx
88		movl -4(%rdi), %esi
89		leaq .L._prints_str0(%rip), %rdi
90		# on x86, al represents the number of SIMD registers used as variadic arguments
91		movb $0, %al
92		call printf@plt
93		movq $0, %rdi
94		call fflush@plt
95		movq %rbp, %rsp
96		popq %rbp
97		ret
98	
99	.section .rodata
100	# length of .L._printc_str0
101		.int 2
102	.L._printc_str0:
103		.asciz "%c"
104	.text
105	_printc:
106		pushq %rbp
107		movq %rsp, %rbp
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		andq $-16, %rsp
110		movb %dil, %sil
111		leaq .L._printc_str0(%rip), %rdi
112		# on x86, al represents the number of SIMD registers used as variadic arguments
113		movb $0, %al
114		call printf@plt
115		movq $0, %rdi
116		call fflush@plt
117		movq %rbp, %rsp
118		popq %rbp
119		ret
120	
121	.section .rodata
122	# length of .L._println_str0
123		.int 0
124	.L._println_str0:
125		.asciz ""
126	.text
127	_println:
128		pushq %rbp
129		movq %rsp, %rbp
130		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
131		andq $-16, %rsp
132		leaq .L._println_str0(%rip), %rdi
133		call puts@plt
134		movq $0, %rdi
135		call fflush@plt
136		movq %rbp, %rsp
137		popq %rbp
138		ret
===========================================================
-- Finished

