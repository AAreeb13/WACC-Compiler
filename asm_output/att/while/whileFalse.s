./valid/while/whileFalse.wacc
calling the reference compiler on ./valid/while/whileFalse.wacc
-- Test: whileFalse.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple unentered while loop

# Output:
# end of loop
#

# Program:

begin
  while false do
    println "looping..."
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileFalse.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 10
4	.L.str0:
5		.asciz "looping..."
6	# length of .L.str1
7		.int 11
8	.L.str1:
9		.asciz "end of loop"
10	.text
11	main:
12		pushq %rbp
13		pushq %rbx
14		movq %rsp, %rbp
15		jmp .L0
16	.L1:
17		# Stack pointer unchanged, no stack allocated arguments
18		leaq .L.str0(%rip), %rax
19		pushq %rax
20		popq %rax
21		movq %rax, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _prints
25		call _println
26	.L0:
27		movq $0, %rax
28		cmpq $1, %rax
29		je .L1
30		# Stack pointer unchanged, no stack allocated arguments
31		leaq .L.str1(%rip), %rax
32		pushq %rax
33		popq %rax
34		movq %rax, %rax
35		movq %rax, %rdi
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		call _println
39		movq $0, %rax
40		popq %rbx
41		popq %rbp
42		ret
43	
44	.section .rodata
45	# length of .L._prints_str0
46		.int 4
47	.L._prints_str0:
48		.asciz "%.*s"
49	.text
50	_prints:
51		pushq %rbp
52		movq %rsp, %rbp
53		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
54		andq $-16, %rsp
55		movq %rdi, %rdx
56		movl -4(%rdi), %esi
57		leaq .L._prints_str0(%rip), %rdi
58		# on x86, al represents the number of SIMD registers used as variadic arguments
59		movb $0, %al
60		call printf@plt
61		movq $0, %rdi
62		call fflush@plt
63		movq %rbp, %rsp
64		popq %rbp
65		ret
66	
67	.section .rodata
68	# length of .L._println_str0
69		.int 0
70	.L._println_str0:
71		.asciz ""
72	.text
73	_println:
74		pushq %rbp
75		movq %rsp, %rbp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		andq $-16, %rsp
78		leaq .L._println_str0(%rip), %rdi
79		call puts@plt
80		movq $0, %rdi
81		call fflush@plt
82		movq %rbp, %rsp
83		popq %rbp
84		ret
===========================================================
-- Finished

