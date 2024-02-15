./valid/array/printRef.wacc
calling the reference compiler on ./valid/array/printRef.wacc
-- Test: printRef.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array (reference) printing

# Output:
# Printing an array variable gives an address, such as #addrs#
#

# Program:

begin
  print "Printing an array variable gives an address, such as " ;
  int[] a = [1,2,3] ;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printRef.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 53
4	.L.str0:
5		.asciz "Printing an array variable gives an address, such as "
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		# Stack pointer unchanged, no stack allocated arguments
16		leaq .L.str0(%rip), %rax
17		pushq %rax
18		popq %rax
19		movq %rax, %rax
20		movq %rax, %rdi
21		# statement primitives do not return results (but will clobber r0/rax)
22		call _prints
23		# 3 element array
24		movl $16, %edi
25		call _malloc
26		movq %rax, %r11
27		# array pointers are shifted forwards by 4 bytes (to account for size)
28		movq %r11, %r11
29		addq $4, %r11
30		movq $3, %rax
31		movl %eax, -4(%r11)
32		movq $1, %rax
33		movl %eax, (%r11)
34		movq $2, %rax
35		movl %eax, 4(%r11)
36		movq $3, %rax
37		movl %eax, 8(%r11)
38		movq %r11, %rax
39		movq %rax, %r12
40		# Stack pointer unchanged, no stack allocated arguments
41		movq %r12, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printp
45		call _println
46		# Stack pointer unchanged, no stack allocated variables
47		movq $0, %rax
48		# popq {%rbx, %r12}
49		movq (%rsp), %rbx
50		movq 8(%rsp), %r12
51		addq $16, %rsp
52		popq %rbp
53		ret
54	
55	.section .rodata
56	# length of .L._prints_str0
57		.int 4
58	.L._prints_str0:
59		.asciz "%.*s"
60	.text
61	_prints:
62		pushq %rbp
63		movq %rsp, %rbp
64		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
65		andq $-16, %rsp
66		movq %rdi, %rdx
67		movl -4(%rdi), %esi
68		leaq .L._prints_str0(%rip), %rdi
69		# on x86, al represents the number of SIMD registers used as variadic arguments
70		movb $0, %al
71		call printf@plt
72		movq $0, %rdi
73		call fflush@plt
74		movq %rbp, %rsp
75		popq %rbp
76		ret
77	
78	.section .rodata
79	# length of .L._printp_str0
80		.int 2
81	.L._printp_str0:
82		.asciz "%p"
83	.text
84	_printp:
85		pushq %rbp
86		movq %rsp, %rbp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		andq $-16, %rsp
89		movq %rdi, %rsi
90		leaq .L._printp_str0(%rip), %rdi
91		# on x86, al represents the number of SIMD registers used as variadic arguments
92		movb $0, %al
93		call printf@plt
94		movq $0, %rdi
95		call fflush@plt
96		movq %rbp, %rsp
97		popq %rbp
98		ret
99	
100	_malloc:
101		pushq %rbp
102		movq %rsp, %rbp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		andq $-16, %rsp
105		call malloc@plt
106		cmpq $0, %rax
107		je _errOutOfMemory
108		movq %rbp, %rsp
109		popq %rbp
110		ret
111	
112	.section .rodata
113	# length of .L._println_str0
114		.int 0
115	.L._println_str0:
116		.asciz ""
117	.text
118	_println:
119		pushq %rbp
120		movq %rsp, %rbp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		andq $-16, %rsp
123		leaq .L._println_str0(%rip), %rdi
124		call puts@plt
125		movq $0, %rdi
126		call fflush@plt
127		movq %rbp, %rsp
128		popq %rbp
129		ret
130	
131	_exit:
132		pushq %rbp
133		movq %rsp, %rbp
134		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
135		andq $-16, %rsp
136		call exit@plt
137		movq %rbp, %rsp
138		popq %rbp
139		ret
140	
141	.section .rodata
142	# length of .L._errOutOfMemory_str0
143		.int 27
144	.L._errOutOfMemory_str0:
145		.asciz "fatal error: out of memory\n"
146	.text
147	_errOutOfMemory:
148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
149		andq $-16, %rsp
150		leaq .L._errOutOfMemory_str0(%rip), %rdi
151		call _prints
152		movb $-1, %dil
153		call exit@plt
===========================================================
-- Finished

