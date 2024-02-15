./valid/function/simple_functions/manyArgumentsChar.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsChar.wacc
-- Test: manyArgumentsChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# A
# b
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  char f(int s, int t, int u, int v, int w, int x, char y, bool z) is
    int i = u + v ;
    int j = w + x ;
    if z
        then return chr (ord y - i * j)
        else return y
    fi
  end

  char r1 = call f(0, 0, 3, 5, 1, 3, 'a', true) ;
  println r1 ;

  char r2 = call f(0, 0, 3, 5, 1, 3, 'b', false) ;
  println r2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsChar.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13}
6		subq $24, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated variables
12		subq $2, %rsp
13		movq $0, %rax
14		movq %rax, %rdi
15		movq $0, %rax
16		movq %rax, %rsi
17		movq $3, %rax
18		movq %rax, %rdx
19		movq $5, %rax
20		movq %rax, %rcx
21		movq $1, %rax
22		movq %rax, %r8
23		movq $3, %rax
24		movq %rax, %r9
25		movq $97, %rax
26		movb %al, 1(%rsp)
27		movq $1, %rax
28		movb %al, (%rsp)
29		call wacc_f
30		movq %rax, %r11
31		addq $2, %rsp
32		movq %r11, %rax
33		movq %rax, %r12
34		# Stack pointer unchanged, no stack allocated arguments
35		movq %r12, %rax
36		movq %rax, %rdi
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _printc
39		call _println
40		subq $2, %rsp
41		movq $0, %rax
42		movq %rax, %rdi
43		movq $0, %rax
44		movq %rax, %rsi
45		movq $3, %rax
46		movq %rax, %rdx
47		movq $5, %rax
48		movq %rax, %rcx
49		movq $1, %rax
50		movq %rax, %r8
51		movq $3, %rax
52		movq %rax, %r9
53		movq $98, %rax
54		movb %al, 1(%rsp)
55		movq $0, %rax
56		movb %al, (%rsp)
57		call wacc_f
58		movq %rax, %r11
59		addq $2, %rsp
60		movq %r11, %rax
61		movq %rax, %r13
62		# Stack pointer unchanged, no stack allocated arguments
63		movq %r13, %rax
64		movq %rax, %rdi
65		# statement primitives do not return results (but will clobber r0/rax)
66		call _printc
67		call _println
68		# Stack pointer unchanged, no stack allocated variables
69		movq $0, %rax
70		# popq {%rbx, %r12, %r13}
71		movq (%rsp), %rbx
72		movq 8(%rsp), %r12
73		movq 16(%rsp), %r13
74		addq $24, %rsp
75		popq %rbp
76		ret
77	
78	wacc_f:
79		pushq %rbp
80		# pushq {%r12, %r13}
81		subq $16, %rsp
82		movq %r12, (%rsp)
83		movq %r13, 8(%rsp)
84		movq %rsp, %rbp
85		# Stack pointer unchanged, no stack allocated variables
86		movl %edx, %eax
87		addl %ecx, %eax
88		jo _errOverflow
89		movslq %eax, %rax
90		pushq %rax
91		popq %rax
92		movq %rax, %rax
93		movq %rax, %r12
94		movl %r8d, %eax
95		addl %r9d, %eax
96		jo _errOverflow
97		movslq %eax, %rax
98		pushq %rax
99		popq %rax
100		movq %rax, %rax
101		movq %rax, %r13
102		movsbq 32(%rbp), %rax
103		cmpq $1, %rax
104		je .L0
105		movsbq 33(%rbp), %rax
106		movq %rax, %rax
107		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
108		movq %rbp, %rsp
109		# popq {%r12, %r13}
110		movq (%rsp), %r12
111		movq 8(%rsp), %r13
112		addq $16, %rsp
113		popq %rbp
114		ret
115		jmp .L1
116	.L0:
117		movl %r12d, %eax
118		imull %r13d, %eax
119		jo _errOverflow
120		movslq %eax, %rax
121		pushq %rax
122		popq %rbx
123		movsbq 33(%rbp), %rax
124		movl %eax, %eax
125		subl %ebx, %eax
126		jo _errOverflow
127		movslq %eax, %rax
128		pushq %rax
129		popq %rax
130		movq %rax, %rax
131		testq $-128, %rax
132		cmovne %rax, %rsi
133		jne _errBadChar
134		pushq %rax
135		popq %rax
136		movq %rax, %rax
137		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
138		movq %rbp, %rsp
139		# popq {%r12, %r13}
140		movq (%rsp), %r12
141		movq 8(%rsp), %r13
142		addq $16, %rsp
143		popq %rbp
144		ret
145	.L1:
146		# Stack pointer unchanged, no stack allocated variables
147		# 'ere be dragons: this is 100% dead code, functions always end in returns!
148	
149	.section .rodata
150	# length of .L._prints_str0
151		.int 4
152	.L._prints_str0:
153		.asciz "%.*s"
154	.text
155	_prints:
156		pushq %rbp
157		movq %rsp, %rbp
158		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
159		andq $-16, %rsp
160		movq %rdi, %rdx
161		movl -4(%rdi), %esi
162		leaq .L._prints_str0(%rip), %rdi
163		# on x86, al represents the number of SIMD registers used as variadic arguments
164		movb $0, %al
165		call printf@plt
166		movq $0, %rdi
167		call fflush@plt
168		movq %rbp, %rsp
169		popq %rbp
170		ret
171	
172	.section .rodata
173	# length of .L._printc_str0
174		.int 2
175	.L._printc_str0:
176		.asciz "%c"
177	.text
178	_printc:
179		pushq %rbp
180		movq %rsp, %rbp
181		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
182		andq $-16, %rsp
183		movb %dil, %sil
184		leaq .L._printc_str0(%rip), %rdi
185		# on x86, al represents the number of SIMD registers used as variadic arguments
186		movb $0, %al
187		call printf@plt
188		movq $0, %rdi
189		call fflush@plt
190		movq %rbp, %rsp
191		popq %rbp
192		ret
193	
194	.section .rodata
195	# length of .L._println_str0
196		.int 0
197	.L._println_str0:
198		.asciz ""
199	.text
200	_println:
201		pushq %rbp
202		movq %rsp, %rbp
203		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
204		andq $-16, %rsp
205		leaq .L._println_str0(%rip), %rdi
206		call puts@plt
207		movq $0, %rdi
208		call fflush@plt
209		movq %rbp, %rsp
210		popq %rbp
211		ret
212	
213	_exit:
214		pushq %rbp
215		movq %rsp, %rbp
216		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
217		andq $-16, %rsp
218		call exit@plt
219		movq %rbp, %rsp
220		popq %rbp
221		ret
222	
223	.section .rodata
224	# length of .L._errBadChar_str0
225		.int 50
226	.L._errBadChar_str0:
227		.asciz "fatal error: int %d is not ascii character 0-127 \n"
228	.text
229	_errBadChar:
230		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
231		andq $-16, %rsp
232		leaq .L._errBadChar_str0(%rip), %rdi
233		# on x86, al represents the number of SIMD registers used as variadic arguments
234		movb $0, %al
235		call printf@plt
236		movq $0, %rdi
237		call fflush@plt
238		movb $-1, %dil
239		call exit@plt
240	
241	.section .rodata
242	# length of .L._errOverflow_str0
243		.int 52
244	.L._errOverflow_str0:
245		.asciz "fatal error: integer overflow or underflow occurred\n"
246	.text
247	_errOverflow:
248		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
249		andq $-16, %rsp
250		leaq .L._errOverflow_str0(%rip), %rdi
251		call _prints
252		movb $-1, %dil
253		call exit@plt
===========================================================
-- Finished

