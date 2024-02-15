./valid/IO/print/hashInProgram.wacc
calling the reference compiler on ./valid/IO/print/hashInProgram.wacc
-- Test: hashInProgram.wacc

-- Uploaded file: 
---------------------------------------------------------------
# In-line comments and printing #

# Output:
# We can print the hash character: #
# We can also print # when its in a string.
#

# Program:

begin
  int x = 0 ; # comments can be in-line
  print "We can print the hash character: " ;
  println '#' ;
  println "We can also print # when its in a string."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
hashInProgram.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 33
4	.L.str0:
5		.asciz "We can print the hash character: "
6	# length of .L.str1
7		.int 41
8	.L.str1:
9		.asciz "We can also print # when its in a string."
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
30		movq $35, %rax
31		movq %rax, %rdi
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _printc
34		call _println
35		# Stack pointer unchanged, no stack allocated arguments
36		leaq .L.str1(%rip), %rax
37		pushq %rax
38		popq %rax
39		movq %rax, %rax
40		movq %rax, %rdi
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _prints
43		call _println
44		# Stack pointer unchanged, no stack allocated variables
45		movq $0, %rax
46		# popq {%rbx, %r12}
47		movq (%rsp), %rbx
48		movq 8(%rsp), %r12
49		addq $16, %rsp
50		popq %rbp
51		ret
52	
53	.section .rodata
54	# length of .L._prints_str0
55		.int 4
56	.L._prints_str0:
57		.asciz "%.*s"
58	.text
59	_prints:
60		pushq %rbp
61		movq %rsp, %rbp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		andq $-16, %rsp
64		movq %rdi, %rdx
65		movl -4(%rdi), %esi
66		leaq .L._prints_str0(%rip), %rdi
67		# on x86, al represents the number of SIMD registers used as variadic arguments
68		movb $0, %al
69		call printf@plt
70		movq $0, %rdi
71		call fflush@plt
72		movq %rbp, %rsp
73		popq %rbp
74		ret
75	
76	.section .rodata
77	# length of .L._printc_str0
78		.int 2
79	.L._printc_str0:
80		.asciz "%c"
81	.text
82	_printc:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		movb %dil, %sil
88		leaq .L._printc_str0(%rip), %rdi
89		# on x86, al represents the number of SIMD registers used as variadic arguments
90		movb $0, %al
91		call printf@plt
92		movq $0, %rdi
93		call fflush@plt
94		movq %rbp, %rsp
95		popq %rbp
96		ret
97	
98	.section .rodata
99	# length of .L._println_str0
100		.int 0
101	.L._println_str0:
102		.asciz ""
103	.text
104	_println:
105		pushq %rbp
106		movq %rsp, %rbp
107		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
108		andq $-16, %rsp
109		leaq .L._println_str0(%rip), %rdi
110		call puts@plt
111		movq $0, %rdi
112		call fflush@plt
113		movq %rbp, %rsp
114		popq %rbp
115		ret
===========================================================
-- Finished

