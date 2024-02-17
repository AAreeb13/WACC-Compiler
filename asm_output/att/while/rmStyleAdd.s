./valid/while/rmStyleAdd.wacc
calling the reference compiler on ./valid/while/rmStyleAdd.wacc
-- Test: rmStyleAdd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# register machine style addition

# Output:
# initial value of x: 3
# (+)(+)(+)(+)(+)(+)(+)
# final value of x: 10
#

# Program:

begin
  int x = 3 ;
  int y = 7 ;
  print "initial value of x: " ;
  println x ;
  while y > 0 do
    print "(+)" ;
    x = x + 1 ;
    y = y - 1
  done ;
  println "" ;
  print "final value of x: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
rmStyleAdd.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 20
4	.L.str0:
5		.asciz "initial value of x: "
6	# length of .L.str1
7		.int 3
8	.L.str1:
9		.asciz "(+)"
10	# length of .L.str2
11		.int 0
12	.L.str2:
13		.asciz ""
14	# length of .L.str3
15		.int 18
16	.L.str3:
17		.asciz "final value of x: "
18	.text
19	main:
20		pushq %rbp
21		# pushq {%rbx, %r12, %r13}
22		subq $24, %rsp
23		movq %rbx, (%rsp)
24		movq %r12, 8(%rsp)
25		movq %r13, 16(%rsp)
26		movq %rsp, %rbp
27		# Stack pointer unchanged, no stack allocated variables
28		movq $3, %rax
29		movq %rax, %r12
30		movq $7, %rax
31		movq %rax, %r13
32		# Stack pointer unchanged, no stack allocated arguments
33		leaq .L.str0(%rip), %rax
34		pushq %rax
35		popq %rax
36		movq %rax, %rax
37		movq %rax, %rdi
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _prints
40		# Stack pointer unchanged, no stack allocated arguments
41		movq %r12, %rax
42		movq %rax, %rdi
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printi
45		call _println
46		jmp .L0
47	.L1:
48		# Stack pointer unchanged, no stack allocated arguments
49		leaq .L.str1(%rip), %rax
50		pushq %rax
51		popq %rax
52		movq %rax, %rax
53		movq %rax, %rdi
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _prints
56		movl %r12d, %eax
57		addl $1, %eax
58		jo _errOverflow
59		movslq %eax, %rax
60		pushq %rax
61		popq %rax
62		movq %rax, %rax
63		movq %rax, %r12
64		movl %r13d, %eax
65		subl $1, %eax
66		jo _errOverflow
67		movslq %eax, %rax
68		pushq %rax
69		popq %rax
70		movq %rax, %rax
71		movq %rax, %r13
72	.L0:
73		cmpq $0, %r13
74		jg .L1
75		# Stack pointer unchanged, no stack allocated arguments
76		leaq .L.str2(%rip), %rax
77		pushq %rax
78		popq %rax
79		movq %rax, %rax
80		movq %rax, %rdi
81		# statement primitives do not return results (but will clobber r0/rax)
82		call _prints
83		call _println
84		# Stack pointer unchanged, no stack allocated arguments
85		leaq .L.str3(%rip), %rax
86		pushq %rax
87		popq %rax
88		movq %rax, %rax
89		movq %rax, %rdi
90		# statement primitives do not return results (but will clobber r0/rax)
91		call _prints
92		# Stack pointer unchanged, no stack allocated arguments
93		movq %r12, %rax
94		movq %rax, %rdi
95		# statement primitives do not return results (but will clobber r0/rax)
96		call _printi
97		call _println
98		# Stack pointer unchanged, no stack allocated variables
99		movq $0, %rax
100		# popq {%rbx, %r12, %r13}
101		movq (%rsp), %rbx
102		movq 8(%rsp), %r12
103		movq 16(%rsp), %r13
104		addq $24, %rsp
105		popq %rbp
106		ret
107	
108	.section .rodata
109	# length of .L._prints_str0
110		.int 4
111	.L._prints_str0:
112		.asciz "%.*s"
113	.text
114	_prints:
115		pushq %rbp
116		movq %rsp, %rbp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		andq $-16, %rsp
119		movq %rdi, %rdx
120		movl -4(%rdi), %esi
121		leaq .L._prints_str0(%rip), %rdi
122		# on x86, al represents the number of SIMD registers used as variadic arguments
123		movb $0, %al
124		call printf@plt
125		movq $0, %rdi
126		call fflush@plt
127		movq %rbp, %rsp
128		popq %rbp
129		ret
130	
131	.section .rodata
132	# length of .L._printi_str0
133		.int 2
134	.L._printi_str0:
135		.asciz "%d"
136	.text
137	_printi:
138		pushq %rbp
139		movq %rsp, %rbp
140		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
141		andq $-16, %rsp
142		movl %edi, %esi
143		leaq .L._printi_str0(%rip), %rdi
144		# on x86, al represents the number of SIMD registers used as variadic arguments
145		movb $0, %al
146		call printf@plt
147		movq $0, %rdi
148		call fflush@plt
149		movq %rbp, %rsp
150		popq %rbp
151		ret
152	
153	.section .rodata
154	# length of .L._println_str0
155		.int 0
156	.L._println_str0:
157		.asciz ""
158	.text
159	_println:
160		pushq %rbp
161		movq %rsp, %rbp
162		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
163		andq $-16, %rsp
164		leaq .L._println_str0(%rip), %rdi
165		call puts@plt
166		movq $0, %rdi
167		call fflush@plt
168		movq %rbp, %rsp
169		popq %rbp
170		ret
171	
172	_exit:
173		pushq %rbp
174		movq %rsp, %rbp
175		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
176		andq $-16, %rsp
177		call exit@plt
178		movq %rbp, %rsp
179		popq %rbp
180		ret
181	
182	.section .rodata
183	# length of .L._errOverflow_str0
184		.int 52
185	.L._errOverflow_str0:
186		.asciz "fatal error: integer overflow or underflow occurred\n"
187	.text
188	_errOverflow:
189		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
190		andq $-16, %rsp
191		leaq .L._errOverflow_str0(%rip), %rdi
192		call _prints
193		movb $-1, %dil
194		call exit@plt
===========================================================
-- Finished

