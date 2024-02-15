./valid/scope/scopeWhileRedefine.wacc
calling the reference compiler on ./valid/scope/scopeWhileRedefine.wacc
-- Test: scopeWhileRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within a while-loop

# Output:
# counting... 5
# counting... 4
# counting... 3
# counting... 2
# counting... 1
# 0 Boom!
#

# Program:

begin
  int x = 5 ;
  string y = " Boom!" ;
  while x > 0 do
    string y = "counting... " ;
    print y;
    println x;
    x = x - 1
  done ;
  print x;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileRedefine.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 6
4	.L.str0:
5		.asciz " Boom!"
6	# length of .L.str1
7		.int 12
8	.L.str1:
9		.asciz "counting... "
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12, %r13, %r14}
14		subq $32, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %r13, 16(%rsp)
18		movq %r14, 24(%rsp)
19		movq %rsp, %rbp
20		# Stack pointer unchanged, no stack allocated variables
21		movq $5, %rax
22		movq %rax, %r12
23		leaq .L.str0(%rip), %rax
24		pushq %rax
25		popq %rax
26		movq %rax, %rax
27		movq %rax, %r13
28		jmp .L0
29	.L1:
30		# Stack pointer unchanged, no stack allocated variables
31		leaq .L.str1(%rip), %rax
32		pushq %rax
33		popq %rax
34		movq %rax, %rax
35		movq %rax, %r14
36		# Stack pointer unchanged, no stack allocated arguments
37		movq %r14, %rax
38		movq %rax, %rdi
39		# statement primitives do not return results (but will clobber r0/rax)
40		call _prints
41		# Stack pointer unchanged, no stack allocated arguments
42		movq %r12, %rax
43		movq %rax, %rdi
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _printi
46		call _println
47		movl %r12d, %eax
48		subl $1, %eax
49		jo _errOverflow
50		movslq %eax, %rax
51		pushq %rax
52		popq %rax
53		movq %rax, %rax
54		movq %rax, %r12
55		# Stack pointer unchanged, no stack allocated variables
56	.L0:
57		cmpq $0, %r12
58		jg .L1
59		# Stack pointer unchanged, no stack allocated arguments
60		movq %r12, %rax
61		movq %rax, %rdi
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _printi
64		# Stack pointer unchanged, no stack allocated arguments
65		movq %r13, %rax
66		movq %rax, %rdi
67		# statement primitives do not return results (but will clobber r0/rax)
68		call _prints
69		call _println
70		# Stack pointer unchanged, no stack allocated variables
71		movq $0, %rax
72		# popq {%rbx, %r12, %r13, %r14}
73		movq (%rsp), %rbx
74		movq 8(%rsp), %r12
75		movq 16(%rsp), %r13
76		movq 24(%rsp), %r14
77		addq $32, %rsp
78		popq %rbp
79		ret
80	
81	.section .rodata
82	# length of .L._prints_str0
83		.int 4
84	.L._prints_str0:
85		.asciz "%.*s"
86	.text
87	_prints:
88		pushq %rbp
89		movq %rsp, %rbp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		andq $-16, %rsp
92		movq %rdi, %rdx
93		movl -4(%rdi), %esi
94		leaq .L._prints_str0(%rip), %rdi
95		# on x86, al represents the number of SIMD registers used as variadic arguments
96		movb $0, %al
97		call printf@plt
98		movq $0, %rdi
99		call fflush@plt
100		movq %rbp, %rsp
101		popq %rbp
102		ret
103	
104	.section .rodata
105	# length of .L._printi_str0
106		.int 2
107	.L._printi_str0:
108		.asciz "%d"
109	.text
110	_printi:
111		pushq %rbp
112		movq %rsp, %rbp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		andq $-16, %rsp
115		movl %edi, %esi
116		leaq .L._printi_str0(%rip), %rdi
117		# on x86, al represents the number of SIMD registers used as variadic arguments
118		movb $0, %al
119		call printf@plt
120		movq $0, %rdi
121		call fflush@plt
122		movq %rbp, %rsp
123		popq %rbp
124		ret
125	
126	.section .rodata
127	# length of .L._println_str0
128		.int 0
129	.L._println_str0:
130		.asciz ""
131	.text
132	_println:
133		pushq %rbp
134		movq %rsp, %rbp
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		andq $-16, %rsp
137		leaq .L._println_str0(%rip), %rdi
138		call puts@plt
139		movq $0, %rdi
140		call fflush@plt
141		movq %rbp, %rsp
142		popq %rbp
143		ret
144	
145	_exit:
146		pushq %rbp
147		movq %rsp, %rbp
148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
149		andq $-16, %rsp
150		call exit@plt
151		movq %rbp, %rsp
152		popq %rbp
153		ret
154	
155	.section .rodata
156	# length of .L._errOverflow_str0
157		.int 52
158	.L._errOverflow_str0:
159		.asciz "fatal error: integer overflow or underflow occurred\n"
160	.text
161	_errOverflow:
162		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
163		andq $-16, %rsp
164		leaq .L._errOverflow_str0(%rip), %rdi
165		call _prints
166		movb $-1, %dil
167		call exit@plt
===========================================================
-- Finished

