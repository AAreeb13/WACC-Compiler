./valid/IO/print/printInt.wacc
calling the reference compiler on ./valid/IO/print/printInt.wacc
-- Test: printInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic Integer printing

# Output:
# An example integer is 189
#

# Program:

begin
  print "An example integer is " ;
  println 189
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printInt.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 22
4	.L.str0:
5		.asciz "An example integer is "
6	.text
7	main:
8		pushq %rbp
9		pushq %rbx
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated arguments
12		leaq .L.str0(%rip), %rax
13		pushq %rax
14		popq %rax
15		movq %rax, %rax
16		movq %rax, %rdi
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _prints
19		# Stack pointer unchanged, no stack allocated arguments
20		movq $189, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _printi
24		call _println
25		movq $0, %rax
26		popq %rbx
27		popq %rbp
28		ret
29	
30	.section .rodata
31	# length of .L._prints_str0
32		.int 4
33	.L._prints_str0:
34		.asciz "%.*s"
35	.text
36	_prints:
37		pushq %rbp
38		movq %rsp, %rbp
39		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
40		andq $-16, %rsp
41		movq %rdi, %rdx
42		movl -4(%rdi), %esi
43		leaq .L._prints_str0(%rip), %rdi
44		# on x86, al represents the number of SIMD registers used as variadic arguments
45		movb $0, %al
46		call printf@plt
47		movq $0, %rdi
48		call fflush@plt
49		movq %rbp, %rsp
50		popq %rbp
51		ret
52	
53	.section .rodata
54	# length of .L._printi_str0
55		.int 2
56	.L._printi_str0:
57		.asciz "%d"
58	.text
59	_printi:
60		pushq %rbp
61		movq %rsp, %rbp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		andq $-16, %rsp
64		movl %edi, %esi
65		leaq .L._printi_str0(%rip), %rdi
66		# on x86, al represents the number of SIMD registers used as variadic arguments
67		movb $0, %al
68		call printf@plt
69		movq $0, %rdi
70		call fflush@plt
71		movq %rbp, %rsp
72		popq %rbp
73		ret
74	
75	.section .rodata
76	# length of .L._println_str0
77		.int 0
78	.L._println_str0:
79		.asciz ""
80	.text
81	_println:
82		pushq %rbp
83		movq %rsp, %rbp
84		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
85		andq $-16, %rsp
86		leaq .L._println_str0(%rip), %rdi
87		call puts@plt
88		movq $0, %rdi
89		call fflush@plt
90		movq %rbp, %rsp
91		popq %rbp
92		ret
===========================================================
-- Finished

