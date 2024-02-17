./valid/IO/print/printCharArray.wacc
calling the reference compiler on ./valid/IO/print/printCharArray.wacc
-- Test: printCharArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# printing the contents of a char[] is possible via an intermediate variable

# Output:
# hi!
#

# Exit:
# 0

# Program:

begin
  char[] s = ['h','i','!'];
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharArray.s contents are:
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
11		# 3 element array
12		movl $7, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $3, %rax
19		movl %eax, -4(%r11)
20		movq $104, %rax
21		movb %al, (%r11)
22		movq $105, %rax
23		movb %al, 1(%r11)
24		movq $33, %rax
25		movb %al, 2(%r11)
26		movq %r11, %rax
27		movq %rax, %r12
28		# Stack pointer unchanged, no stack allocated arguments
29		movq %r12, %rax
30		movq %rax, %rdi
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _prints
33		call _println
34		# Stack pointer unchanged, no stack allocated variables
35		movq $0, %rax
36		# popq {%rbx, %r12}
37		movq (%rsp), %rbx
38		movq 8(%rsp), %r12
39		addq $16, %rsp
40		popq %rbp
41		ret
42	
43	.section .rodata
44	# length of .L._prints_str0
45		.int 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.text
49	_prints:
50		pushq %rbp
51		movq %rsp, %rbp
52		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
53		andq $-16, %rsp
54		movq %rdi, %rdx
55		movl -4(%rdi), %esi
56		leaq .L._prints_str0(%rip), %rdi
57		# on x86, al represents the number of SIMD registers used as variadic arguments
58		movb $0, %al
59		call printf@plt
60		movq $0, %rdi
61		call fflush@plt
62		movq %rbp, %rsp
63		popq %rbp
64		ret
65	
66	_malloc:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		call malloc@plt
72		cmpq $0, %rax
73		je _errOutOfMemory
74		movq %rbp, %rsp
75		popq %rbp
76		ret
77	
78	.section .rodata
79	# length of .L._println_str0
80		.int 0
81	.L._println_str0:
82		.asciz ""
83	.text
84	_println:
85		pushq %rbp
86		movq %rsp, %rbp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		andq $-16, %rsp
89		leaq .L._println_str0(%rip), %rdi
90		call puts@plt
91		movq $0, %rdi
92		call fflush@plt
93		movq %rbp, %rsp
94		popq %rbp
95		ret
96	
97	_exit:
98		pushq %rbp
99		movq %rsp, %rbp
100		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
101		andq $-16, %rsp
102		call exit@plt
103		movq %rbp, %rsp
104		popq %rbp
105		ret
106	
107	.section .rodata
108	# length of .L._errOutOfMemory_str0
109		.int 27
110	.L._errOutOfMemory_str0:
111		.asciz "fatal error: out of memory\n"
112	.text
113	_errOutOfMemory:
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		andq $-16, %rsp
116		leaq .L._errOutOfMemory_str0(%rip), %rdi
117		call _prints
118		movb $-1, %dil
119		call exit@plt
===========================================================
-- Finished

