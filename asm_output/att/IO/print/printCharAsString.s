./valid/IO/print/printCharAsString.wacc
calling the reference compiler on ./valid/IO/print/printCharAsString.wacc
-- Test: printCharAsString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# character array treated as a string

# Output:
# foo
# bar
#

# Program:

begin
  char[] str = ['f','o','o'];
  println str;
  str = ['b','a','r'];
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharAsString.s contents are:
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
20		movq $102, %rax
21		movb %al, (%r11)
22		movq $111, %rax
23		movb %al, 1(%r11)
24		movq $111, %rax
25		movb %al, 2(%r11)
26		movq %r11, %rax
27		movq %rax, %r12
28		# Stack pointer unchanged, no stack allocated arguments
29		movq %r12, %rax
30		movq %rax, %rdi
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _prints
33		call _println
34		# 3 element array
35		movl $7, %edi
36		call _malloc
37		movq %rax, %r11
38		# array pointers are shifted forwards by 4 bytes (to account for size)
39		movq %r11, %r11
40		addq $4, %r11
41		movq $3, %rax
42		movl %eax, -4(%r11)
43		movq $98, %rax
44		movb %al, (%r11)
45		movq $97, %rax
46		movb %al, 1(%r11)
47		movq $114, %rax
48		movb %al, 2(%r11)
49		movq %r11, %rax
50		movq %rax, %r12
51		# Stack pointer unchanged, no stack allocated arguments
52		movq %r12, %rax
53		movq %rax, %rdi
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _prints
56		call _println
57		# Stack pointer unchanged, no stack allocated variables
58		movq $0, %rax
59		# popq {%rbx, %r12}
60		movq (%rsp), %rbx
61		movq 8(%rsp), %r12
62		addq $16, %rsp
63		popq %rbp
64		ret
65	
66	.section .rodata
67	# length of .L._prints_str0
68		.int 4
69	.L._prints_str0:
70		.asciz "%.*s"
71	.text
72	_prints:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		movq %rdi, %rdx
78		movl -4(%rdi), %esi
79		leaq .L._prints_str0(%rip), %rdi
80		# on x86, al represents the number of SIMD registers used as variadic arguments
81		movb $0, %al
82		call printf@plt
83		movq $0, %rdi
84		call fflush@plt
85		movq %rbp, %rsp
86		popq %rbp
87		ret
88	
89	_malloc:
90		pushq %rbp
91		movq %rsp, %rbp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		call malloc@plt
95		cmpq $0, %rax
96		je _errOutOfMemory
97		movq %rbp, %rsp
98		popq %rbp
99		ret
100	
101	.section .rodata
102	# length of .L._println_str0
103		.int 0
104	.L._println_str0:
105		.asciz ""
106	.text
107	_println:
108		pushq %rbp
109		movq %rsp, %rbp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		andq $-16, %rsp
112		leaq .L._println_str0(%rip), %rdi
113		call puts@plt
114		movq $0, %rdi
115		call fflush@plt
116		movq %rbp, %rsp
117		popq %rbp
118		ret
119	
120	_exit:
121		pushq %rbp
122		movq %rsp, %rbp
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		andq $-16, %rsp
125		call exit@plt
126		movq %rbp, %rsp
127		popq %rbp
128		ret
129	
130	.section .rodata
131	# length of .L._errOutOfMemory_str0
132		.int 27
133	.L._errOutOfMemory_str0:
134		.asciz "fatal error: out of memory\n"
135	.text
136	_errOutOfMemory:
137		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
138		andq $-16, %rsp
139		leaq .L._errOutOfMemory_str0(%rip), %rdi
140		call _prints
141		movb $-1, %dil
142		call exit@plt
===========================================================
-- Finished

