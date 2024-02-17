./valid/pairs/pairarray.wacc
calling the reference compiler on ./valid/pairs/pairarray.wacc
-- Test: pairarray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that pairs can be unpacked directly from arrays

# Output:
# 3
#

# Program:

begin
  pair(int, int) p = newpair(5, 6);
  pair(int, int)[] a = [p, p];
  fst a[0] = 3 ;
  int x = fst a[1] ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairarray.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14}
6		subq $32, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %rsp, %rbp
12		# Stack pointer unchanged, no stack allocated variables
13		movl $16, %edi
14		call _malloc
15		movq %rax, %r11
16		movq $5, %rax
17		movq %rax, (%r11)
18		movq $6, %rax
19		movq %rax, 8(%r11)
20		movq %r11, %rax
21		movq %rax, %r12
22		# 2 element array
23		movl $20, %edi
24		call _malloc
25		movq %rax, %r11
26		# array pointers are shifted forwards by 4 bytes (to account for size)
27		movq %r11, %r11
28		addq $4, %r11
29		movq $2, %rax
30		movl %eax, -4(%r11)
31		movq %r12, %rax
32		movq %rax, (%r11)
33		movq %r12, %rax
34		movq %rax, 8(%r11)
35		movq %r11, %rax
36		movq %rax, %r13
37		movl $0, %r10d
38		movq %r13, %r9
39		call _arrLoad8
40		movq %r9, %rax
41		movq %rax, %rax
42		pushq %rax
43		popq %rbx
44		cmpq $0, %rbx
45		je _errNull
46		movq $3, %rax
47		movq %rax, (%rbx)
48		movl $1, %r10d
49		movq %r13, %r9
50		call _arrLoad8
51		movq %r9, %rax
52		movq %rax, %rax
53		pushq %rax
54		popq %rax
55		cmpq $0, %rax
56		je _errNull
57		movq (%rax), %rax
58		movq %rax, %rax
59		movq %rax, %r14
60		# Stack pointer unchanged, no stack allocated arguments
61		movq %r14, %rax
62		movq %rax, %rdi
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _printi
65		call _println
66		# Stack pointer unchanged, no stack allocated variables
67		movq $0, %rax
68		# popq {%rbx, %r12, %r13, %r14}
69		movq (%rsp), %rbx
70		movq 8(%rsp), %r12
71		movq 16(%rsp), %r13
72		movq 24(%rsp), %r14
73		addq $32, %rsp
74		popq %rbp
75		ret
76	
77	.section .rodata
78	# length of .L._prints_str0
79		.int 4
80	.L._prints_str0:
81		.asciz "%.*s"
82	.text
83	_prints:
84		pushq %rbp
85		movq %rsp, %rbp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		andq $-16, %rsp
88		movq %rdi, %rdx
89		movl -4(%rdi), %esi
90		leaq .L._prints_str0(%rip), %rdi
91		# on x86, al represents the number of SIMD registers used as variadic arguments
92		movb $0, %al
93		call printf@plt
94		movq $0, %rdi
95		call fflush@plt
96		movq %rbp, %rsp
97		popq %rbp
98		ret
99	
100	_malloc:
101		pushq %rbp
102		movq %rsp, %rbp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		andq $-16, %rsp
105		call malloc@plt
106		cmpq $0, %rax
107		je _errOutOfMemory
108		movq %rbp, %rsp
109		popq %rbp
110		ret
111	
112	.section .rodata
113	# length of .L._printi_str0
114		.int 2
115	.L._printi_str0:
116		.asciz "%d"
117	.text
118	_printi:
119		pushq %rbp
120		movq %rsp, %rbp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		andq $-16, %rsp
123		movl %edi, %esi
124		leaq .L._printi_str0(%rip), %rdi
125		# on x86, al represents the number of SIMD registers used as variadic arguments
126		movb $0, %al
127		call printf@plt
128		movq $0, %rdi
129		call fflush@plt
130		movq %rbp, %rsp
131		popq %rbp
132		ret
133	
134	.section .rodata
135	# length of .L._println_str0
136		.int 0
137	.L._println_str0:
138		.asciz ""
139	.text
140	_println:
141		pushq %rbp
142		movq %rsp, %rbp
143		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
144		andq $-16, %rsp
145		leaq .L._println_str0(%rip), %rdi
146		call puts@plt
147		movq $0, %rdi
148		call fflush@plt
149		movq %rbp, %rsp
150		popq %rbp
151		ret
152	
153	_exit:
154		pushq %rbp
155		movq %rsp, %rbp
156		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
157		andq $-16, %rsp
158		call exit@plt
159		movq %rbp, %rsp
160		popq %rbp
161		ret
162	
163	_arrLoad8:
164		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
165		pushq %rbx
166		cmpl $0, %r10d
167		cmovl %r10d, %esi
168		jl _errOutOfBounds
169		movl -4(%r9), %ebx
170		cmpl %ebx, %r10d
171		cmovge %r10d, %esi
172		jge _errOutOfBounds
173		movq (%r9,%r10,8), %r9
174		popq %rbx
175		ret
176	
177	.section .rodata
178	# length of .L._errOutOfMemory_str0
179		.int 27
180	.L._errOutOfMemory_str0:
181		.asciz "fatal error: out of memory\n"
182	.text
183	_errOutOfMemory:
184		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
185		andq $-16, %rsp
186		leaq .L._errOutOfMemory_str0(%rip), %rdi
187		call _prints
188		movb $-1, %dil
189		call exit@plt
190	
191	.section .rodata
192	# length of .L._errNull_str0
193		.int 45
194	.L._errNull_str0:
195		.asciz "fatal error: null pair dereferenced or freed\n"
196	.text
197	_errNull:
198		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
199		andq $-16, %rsp
200		leaq .L._errNull_str0(%rip), %rdi
201		call _prints
202		movb $-1, %dil
203		call exit@plt
204	
205	.section .rodata
206	# length of .L._errOutOfBounds_str0
207		.int 42
208	.L._errOutOfBounds_str0:
209		.asciz "fatal error: array index %d out of bounds\n"
210	.text
211	_errOutOfBounds:
212		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
213		andq $-16, %rsp
214		leaq .L._errOutOfBounds_str0(%rip), %rdi
215		# on x86, al represents the number of SIMD registers used as variadic arguments
216		movb $0, %al
217		call printf@plt
218		movq $0, %rdi
219		call fflush@plt
220		movb $-1, %dil
221		call exit@plt
===========================================================
-- Finished

