./valid/IO/print/printBool.wacc
calling the reference compiler on ./valid/IO/print/printBool.wacc
-- Test: printBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic Boolean printing

# Output:
# True is true
# False is false
#

# Program:

begin
  print "True is " ;
  println true ;

  print "False is " ;
  println false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printBool.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 8
4	.L.str0:
5		.asciz "True is "
6	# length of .L.str1
7		.int 9
8	.L.str1:
9		.asciz "False is "
10	.text
11	main:
12		pushq %rbp
13		pushq %rbx
14		movq %rsp, %rbp
15		# Stack pointer unchanged, no stack allocated arguments
16		leaq .L.str0(%rip), %rax
17		pushq %rax
18		popq %rax
19		movq %rax, %rax
20		movq %rax, %rdi
21		# statement primitives do not return results (but will clobber r0/rax)
22		call _prints
23		# Stack pointer unchanged, no stack allocated arguments
24		movq $1, %rax
25		movq %rax, %rdi
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printb
28		call _println
29		# Stack pointer unchanged, no stack allocated arguments
30		leaq .L.str1(%rip), %rax
31		pushq %rax
32		popq %rax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _prints
37		# Stack pointer unchanged, no stack allocated arguments
38		movq $0, %rax
39		movq %rax, %rdi
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _printb
42		call _println
43		movq $0, %rax
44		popq %rbx
45		popq %rbp
46		ret
47	
48	.section .rodata
49	# length of .L._prints_str0
50		.int 4
51	.L._prints_str0:
52		.asciz "%.*s"
53	.text
54	_prints:
55		pushq %rbp
56		movq %rsp, %rbp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		andq $-16, %rsp
59		movq %rdi, %rdx
60		movl -4(%rdi), %esi
61		leaq .L._prints_str0(%rip), %rdi
62		# on x86, al represents the number of SIMD registers used as variadic arguments
63		movb $0, %al
64		call printf@plt
65		movq $0, %rdi
66		call fflush@plt
67		movq %rbp, %rsp
68		popq %rbp
69		ret
70	
71	.section .rodata
72	# length of .L._printb_str0
73		.int 5
74	.L._printb_str0:
75		.asciz "false"
76	# length of .L._printb_str1
77		.int 4
78	.L._printb_str1:
79		.asciz "true"
80	# length of .L._printb_str2
81		.int 4
82	.L._printb_str2:
83		.asciz "%.*s"
84	.text
85	_printb:
86		pushq %rbp
87		movq %rsp, %rbp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		andq $-16, %rsp
90		cmpb $0, %dil
91		jne .L_printb0
92		leaq .L._printb_str0(%rip), %rdx
93		jmp .L_printb1
94	.L_printb0:
95		leaq .L._printb_str1(%rip), %rdx
96	.L_printb1:
97		movl -4(%rdx), %esi
98		leaq .L._printb_str2(%rip), %rdi
99		# on x86, al represents the number of SIMD registers used as variadic arguments
100		movb $0, %al
101		call printf@plt
102		movq $0, %rdi
103		call fflush@plt
104		movq %rbp, %rsp
105		popq %rbp
106		ret
107	
108	.section .rodata
109	# length of .L._println_str0
110		.int 0
111	.L._println_str0:
112		.asciz ""
113	.text
114	_println:
115		pushq %rbp
116		movq %rsp, %rbp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		andq $-16, %rsp
119		leaq .L._println_str0(%rip), %rdi
120		call puts@plt
121		movq $0, %rdi
122		call fflush@plt
123		movq %rbp, %rsp
124		popq %rbp
125		ret
===========================================================
-- Finished

