./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
calling the reference compiler on ./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
-- Test: usesArgumentWhilstMakingArgument.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 12
# -4
# 32
#

# Program:

begin
  int f(int u, int v) is
    int x = call g(u + v, u - v, u * v) ;
    return x
  end

  int g(int x, int y, int z) is
    println(x) ;
    println(y) ;
    println(z) ;
    return 0
  end

  int r = call f(4, 8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
usesArgumentWhilstMakingArgument.s contents are:
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
11		# Stack pointer unchanged, no stack allocated arguments
12		movq $4, %rax
13		movq %rax, %rdi
14		movq $8, %rax
15		movq %rax, %rsi
16		call wacc_f
17		movq %rax, %r11
18		# Stack pointer unchanged, no stack allocated arguments
19		movq %r11, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated variables
22		movq $0, %rax
23		# popq {%rbx, %r12}
24		movq (%rsp), %rbx
25		movq 8(%rsp), %r12
26		addq $16, %rsp
27		popq %rbp
28		ret
29	
30	wacc_f:
31		pushq %rbp
32		pushq %r12
33		movq %rsp, %rbp
34		# Stack pointer unchanged, no stack allocated variables
35		# pushq {%rsi, %rdi}
36		subq $16, %rsp
37		movq %rsi, (%rsp)
38		movq %rdi, 8(%rsp)
39		# Set up R11 as a temporary second base pointer for the caller saved things
40		movq %rsp, %r11
41		# Stack pointer unchanged, no stack allocated arguments
42		movl %edi, %eax
43		addl %esi, %eax
44		jo _errOverflow
45		movslq %eax, %rax
46		pushq %rax
47		popq %rax
48		movq %rax, %rax
49		movq %rax, %rdi
50		movq 8(%r11), %rax
51		movl %eax, %eax
52		subl %esi, %eax
53		jo _errOverflow
54		movslq %eax, %rax
55		pushq %rax
56		popq %rax
57		movq %rax, %rax
58		movq %rax, %rsi
59		movq (%r11), %rbx
60		movq 8(%r11), %rax
61		movl %eax, %eax
62		imull %ebx, %eax
63		jo _errOverflow
64		movslq %eax, %rax
65		pushq %rax
66		popq %rax
67		movq %rax, %rax
68		movq %rax, %rdx
69		call wacc_g
70		movq %rax, %r11
71		# Stack pointer unchanged, no stack allocated arguments
72		# popq {%rsi, %rdi}
73		movq (%rsp), %rsi
74		movq 8(%rsp), %rdi
75		addq $16, %rsp
76		movq %r11, %rax
77		movq %rax, %r12
78		movq %r12, %rax
79		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
80		movq %rbp, %rsp
81		popq %r12
82		popq %rbp
83		ret
84		# Stack pointer unchanged, no stack allocated variables
85		# 'ere be dragons: this is 100% dead code, functions always end in returns!
86	
87	wacc_g:
88		pushq %rbp
89		movq %rsp, %rbp
90		# pushq {%rdx, %rsi, %rdi}
91		subq $24, %rsp
92		movq %rdx, (%rsp)
93		movq %rsi, 8(%rsp)
94		movq %rdi, 16(%rsp)
95		# Set up R11 as a temporary second base pointer for the caller saved things
96		movq %rsp, %r11
97		# Stack pointer unchanged, no stack allocated arguments
98		movq %rdi, %rax
99		movq %rax, %rdi
100		# statement primitives do not return results (but will clobber r0/rax)
101		call _printi
102		call _println
103		# popq {%rdx, %rsi, %rdi}
104		movq (%rsp), %rdx
105		movq 8(%rsp), %rsi
106		movq 16(%rsp), %rdi
107		addq $24, %rsp
108		# pushq {%rdx, %rsi, %rdi}
109		subq $24, %rsp
110		movq %rdx, (%rsp)
111		movq %rsi, 8(%rsp)
112		movq %rdi, 16(%rsp)
113		# Set up R11 as a temporary second base pointer for the caller saved things
114		movq %rsp, %r11
115		# Stack pointer unchanged, no stack allocated arguments
116		movq %rsi, %rax
117		movq %rax, %rdi
118		# statement primitives do not return results (but will clobber r0/rax)
119		call _printi
120		call _println
121		# popq {%rdx, %rsi, %rdi}
122		movq (%rsp), %rdx
123		movq 8(%rsp), %rsi
124		movq 16(%rsp), %rdi
125		addq $24, %rsp
126		# pushq {%rdx, %rsi, %rdi}
127		subq $24, %rsp
128		movq %rdx, (%rsp)
129		movq %rsi, 8(%rsp)
130		movq %rdi, 16(%rsp)
131		# Set up R11 as a temporary second base pointer for the caller saved things
132		movq %rsp, %r11
133		# Stack pointer unchanged, no stack allocated arguments
134		movq %rdx, %rax
135		movq %rax, %rdi
136		# statement primitives do not return results (but will clobber r0/rax)
137		call _printi
138		call _println
139		# popq {%rdx, %rsi, %rdi}
140		movq (%rsp), %rdx
141		movq 8(%rsp), %rsi
142		movq 16(%rsp), %rdi
143		addq $24, %rsp
144		movq $0, %rax
145		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
146		movq %rbp, %rsp
147		popq %rbp
148		ret
149		# 'ere be dragons: this is 100% dead code, functions always end in returns!
150	
151	.section .rodata
152	# length of .L._prints_str0
153		.int 4
154	.L._prints_str0:
155		.asciz "%.*s"
156	.text
157	_prints:
158		pushq %rbp
159		movq %rsp, %rbp
160		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
161		andq $-16, %rsp
162		movq %rdi, %rdx
163		movl -4(%rdi), %esi
164		leaq .L._prints_str0(%rip), %rdi
165		# on x86, al represents the number of SIMD registers used as variadic arguments
166		movb $0, %al
167		call printf@plt
168		movq $0, %rdi
169		call fflush@plt
170		movq %rbp, %rsp
171		popq %rbp
172		ret
173	
174	.section .rodata
175	# length of .L._printi_str0
176		.int 2
177	.L._printi_str0:
178		.asciz "%d"
179	.text
180	_printi:
181		pushq %rbp
182		movq %rsp, %rbp
183		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
184		andq $-16, %rsp
185		movl %edi, %esi
186		leaq .L._printi_str0(%rip), %rdi
187		# on x86, al represents the number of SIMD registers used as variadic arguments
188		movb $0, %al
189		call printf@plt
190		movq $0, %rdi
191		call fflush@plt
192		movq %rbp, %rsp
193		popq %rbp
194		ret
195	
196	.section .rodata
197	# length of .L._println_str0
198		.int 0
199	.L._println_str0:
200		.asciz ""
201	.text
202	_println:
203		pushq %rbp
204		movq %rsp, %rbp
205		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
206		andq $-16, %rsp
207		leaq .L._println_str0(%rip), %rdi
208		call puts@plt
209		movq $0, %rdi
210		call fflush@plt
211		movq %rbp, %rsp
212		popq %rbp
213		ret
214	
215	_exit:
216		pushq %rbp
217		movq %rsp, %rbp
218		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
219		andq $-16, %rsp
220		call exit@plt
221		movq %rbp, %rsp
222		popq %rbp
223		ret
224	
225	.section .rodata
226	# length of .L._errOverflow_str0
227		.int 52
228	.L._errOverflow_str0:
229		.asciz "fatal error: integer overflow or underflow occurred\n"
230	.text
231	_errOverflow:
232		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
233		andq $-16, %rsp
234		leaq .L._errOverflow_str0(%rip), %rdi
235		call _prints
236		movb $-1, %dil
237		call exit@plt
===========================================================
-- Finished

