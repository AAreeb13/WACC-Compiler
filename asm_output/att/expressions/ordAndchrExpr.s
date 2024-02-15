./valid/expressions/ordAndchrExpr.wacc
calling the reference compiler on ./valid/expressions/ordAndchrExpr.wacc
-- Test: ordAndchrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evalutaing ord and chr

# Output:
# a is 97
# 99 is c
#

# Program:

begin
  char a = 'a' ;
  int i = 99 ;

  print a ;
  print " is " ;
  println ord a ;

  print i ;
  print " is " ;
  println chr i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ordAndchrExpr.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 4
4	.L.str0:
5		.asciz " is "
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12, %r13}
10		subq $24, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %r13, 16(%rsp)
14		movq %rsp, %rbp
15		# Stack pointer unchanged, no stack allocated variables
16		movq $97, %rax
17		movq %rax, %r12
18		movq $99, %rax
19		movq %rax, %r13
20		# Stack pointer unchanged, no stack allocated arguments
21		movq %r12, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printc
25		# Stack pointer unchanged, no stack allocated arguments
26		leaq .L.str0(%rip), %rax
27		pushq %rax
28		popq %rax
29		movq %rax, %rax
30		movq %rax, %rdi
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _prints
33		# Stack pointer unchanged, no stack allocated arguments
34		movq %r12, %rax
35		movq %rax, %rdi
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _printi
38		call _println
39		# Stack pointer unchanged, no stack allocated arguments
40		movq %r13, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printi
44		# Stack pointer unchanged, no stack allocated arguments
45		leaq .L.str0(%rip), %rax
46		pushq %rax
47		popq %rax
48		movq %rax, %rax
49		movq %rax, %rdi
50		# statement primitives do not return results (but will clobber r0/rax)
51		call _prints
52		# Stack pointer unchanged, no stack allocated arguments
53		movq %r13, %rax
54		testq $-128, %rax
55		cmovne %rax, %rsi
56		jne _errBadChar
57		pushq %rax
58		popq %rax
59		movq %rax, %rax
60		movq %rax, %rdi
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _printc
63		call _println
64		# Stack pointer unchanged, no stack allocated variables
65		movq $0, %rax
66		# popq {%rbx, %r12, %r13}
67		movq (%rsp), %rbx
68		movq 8(%rsp), %r12
69		movq 16(%rsp), %r13
70		addq $24, %rsp
71		popq %rbp
72		ret
73	
74	.section .rodata
75	# length of .L._prints_str0
76		.int 4
77	.L._prints_str0:
78		.asciz "%.*s"
79	.text
80	_prints:
81		pushq %rbp
82		movq %rsp, %rbp
83		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
84		andq $-16, %rsp
85		movq %rdi, %rdx
86		movl -4(%rdi), %esi
87		leaq .L._prints_str0(%rip), %rdi
88		# on x86, al represents the number of SIMD registers used as variadic arguments
89		movb $0, %al
90		call printf@plt
91		movq $0, %rdi
92		call fflush@plt
93		movq %rbp, %rsp
94		popq %rbp
95		ret
96	
97	.section .rodata
98	# length of .L._printi_str0
99		.int 2
100	.L._printi_str0:
101		.asciz "%d"
102	.text
103	_printi:
104		pushq %rbp
105		movq %rsp, %rbp
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		andq $-16, %rsp
108		movl %edi, %esi
109		leaq .L._printi_str0(%rip), %rdi
110		# on x86, al represents the number of SIMD registers used as variadic arguments
111		movb $0, %al
112		call printf@plt
113		movq $0, %rdi
114		call fflush@plt
115		movq %rbp, %rsp
116		popq %rbp
117		ret
118	
119	.section .rodata
120	# length of .L._printc_str0
121		.int 2
122	.L._printc_str0:
123		.asciz "%c"
124	.text
125	_printc:
126		pushq %rbp
127		movq %rsp, %rbp
128		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
129		andq $-16, %rsp
130		movb %dil, %sil
131		leaq .L._printc_str0(%rip), %rdi
132		# on x86, al represents the number of SIMD registers used as variadic arguments
133		movb $0, %al
134		call printf@plt
135		movq $0, %rdi
136		call fflush@plt
137		movq %rbp, %rsp
138		popq %rbp
139		ret
140	
141	.section .rodata
142	# length of .L._println_str0
143		.int 0
144	.L._println_str0:
145		.asciz ""
146	.text
147	_println:
148		pushq %rbp
149		movq %rsp, %rbp
150		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
151		andq $-16, %rsp
152		leaq .L._println_str0(%rip), %rdi
153		call puts@plt
154		movq $0, %rdi
155		call fflush@plt
156		movq %rbp, %rsp
157		popq %rbp
158		ret
159	
160	_exit:
161		pushq %rbp
162		movq %rsp, %rbp
163		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
164		andq $-16, %rsp
165		call exit@plt
166		movq %rbp, %rsp
167		popq %rbp
168		ret
169	
170	.section .rodata
171	# length of .L._errBadChar_str0
172		.int 50
173	.L._errBadChar_str0:
174		.asciz "fatal error: int %d is not ascii character 0-127 \n"
175	.text
176	_errBadChar:
177		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
178		andq $-16, %rsp
179		leaq .L._errBadChar_str0(%rip), %rdi
180		# on x86, al represents the number of SIMD registers used as variadic arguments
181		movb $0, %al
182		call printf@plt
183		movq $0, %rdi
184		call fflush@plt
185		movb $-1, %dil
186		call exit@plt
===========================================================
-- Finished

