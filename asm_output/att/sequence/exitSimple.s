./valid/sequence/exitSimple.wacc
calling the reference compiler on ./valid/sequence/exitSimple.wacc
-- Test: exitSimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# exit with unreachable print

# Output:

# Exit:
# 42

# Program:

begin
  exit 42 ;
  println "Should not print this."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exitSimple.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 22
4	.L.str0:
5		.asciz "Should not print this."
6	.text
7	main:
8		pushq %rbp
9		pushq %rbx
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated arguments
12		movq $42, %rax
13		movq %rax, %rdi
14		# statement primitives do not return results (but will clobber r0/rax)
15		call _exit
16		# Stack pointer unchanged, no stack allocated arguments
17		leaq .L.str0(%rip), %rax
18		pushq %rax
19		popq %rax
20		movq %rax, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _prints
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
54	# length of .L._println_str0
55		.int 0
56	.L._println_str0:
57		.asciz ""
58	.text
59	_println:
60		pushq %rbp
61		movq %rsp, %rbp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		andq $-16, %rsp
64		leaq .L._println_str0(%rip), %rdi
65		call puts@plt
66		movq $0, %rdi
67		call fflush@plt
68		movq %rbp, %rsp
69		popq %rbp
70		ret
71	
72	_exit:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		call exit@plt
78		movq %rbp, %rsp
79		popq %rbp
80		ret
===========================================================
-- Finished

