./valid/IO/IOSequence.wacc
calling the reference compiler on ./valid/IO/IOSequence.wacc
-- Test: IOSequence.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic input/output sequence

# Input: 37

# Output:
# Please input an integer: You input: 37
#

# Program:

begin
  int x = 0 ;
  print "Please input an integer: " ;
  read x ;
  print "You input: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOSequence.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 25
4	.L.str0:
5		.asciz "Please input an integer: "
6	# length of .L.str1
7		.int 11
8	.L.str1:
9		.asciz "You input: "
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $0, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated arguments
22		leaq .L.str0(%rip), %rax
23		pushq %rax
24		popq %rax
25		movq %rax, %rax
26		movq %rax, %rdi
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _prints
29		# Stack pointer unchanged, no stack allocated arguments
30		# load the current value in the destination of the read so it supports defaults
31		movq %r12, %rax
32		movq %rax, %rdi
33		call _readi
34		movq %rax, %r11
35		movq %r11, %rax
36		movq %rax, %r12
37		# Stack pointer unchanged, no stack allocated arguments
38		leaq .L.str1(%rip), %rax
39		pushq %rax
40		popq %rax
41		movq %rax, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		# Stack pointer unchanged, no stack allocated arguments
46		movq %r12, %rax
47		movq %rax, %rdi
48		# statement primitives do not return results (but will clobber r0/rax)
49		call _printi
50		call _println
51		# Stack pointer unchanged, no stack allocated variables
52		movq $0, %rax
53		# popq {%rbx, %r12}
54		movq (%rsp), %rbx
55		movq 8(%rsp), %r12
56		addq $16, %rsp
57		popq %rbp
58		ret
59	
60	.section .rodata
61	# length of .L._prints_str0
62		.int 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.text
66	_prints:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		movq %rdi, %rdx
72		movl -4(%rdi), %esi
73		leaq .L._prints_str0(%rip), %rdi
74		# on x86, al represents the number of SIMD registers used as variadic arguments
75		movb $0, %al
76		call printf@plt
77		movq $0, %rdi
78		call fflush@plt
79		movq %rbp, %rsp
80		popq %rbp
81		ret
82	
83	.section .rodata
84	# length of .L._readi_str0
85		.int 2
86	.L._readi_str0:
87		.asciz "%d"
88	.text
89	_readi:
90		pushq %rbp
91		movq %rsp, %rbp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		# RDI contains the "original" value of the destination of the read
95		# allocate space on the stack to store the read: preserve alignment!
96		# the passed default argument should be stored in case of EOF
97		subq $16, %rsp
98		movl %edi, (%rsp)
99		leaq (%rsp), %rsi
100		leaq .L._readi_str0(%rip), %rdi
101		# on x86, al represents the number of SIMD registers used as variadic arguments
102		movb $0, %al
103		call scanf@plt
104		movslq (%rsp), %rax
105		addq $16, %rsp
106		movq %rbp, %rsp
107		popq %rbp
108		ret
109	
110	.section .rodata
111	# length of .L._printi_str0
112		.int 2
113	.L._printi_str0:
114		.asciz "%d"
115	.text
116	_printi:
117		pushq %rbp
118		movq %rsp, %rbp
119		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
120		andq $-16, %rsp
121		movl %edi, %esi
122		leaq .L._printi_str0(%rip), %rdi
123		# on x86, al represents the number of SIMD registers used as variadic arguments
124		movb $0, %al
125		call printf@plt
126		movq $0, %rdi
127		call fflush@plt
128		movq %rbp, %rsp
129		popq %rbp
130		ret
131	
132	.section .rodata
133	# length of .L._println_str0
134		.int 0
135	.L._println_str0:
136		.asciz ""
137	.text
138	_println:
139		pushq %rbp
140		movq %rsp, %rbp
141		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
142		andq $-16, %rsp
143		leaq .L._println_str0(%rip), %rdi
144		call puts@plt
145		movq $0, %rdi
146		call fflush@plt
147		movq %rbp, %rsp
148		popq %rbp
149		ret
===========================================================
-- Finished

