./valid/function/nested_functions/printTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printTriangle.wacc
-- Test: printTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a fixed size triangle

# Output:
# --------
# -------
# ------
# -----
# ----
# ---
# --
# -
#

# Program:

begin
  int f(int x) is
    if x == 0 then
      skip
    else
      int i = x ;
      while i > 0 do
        print "-" ;
        i = i - 1
      done ;
      println "" ;
      int s = call f(x - 1)
    fi ;
    return 0
  end

  int s = call f(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printTriangle.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 1
4	.L.str0:
5		.asciz "-"
6	# length of .L.str1
7		.int 0
8	.L.str1:
9		.asciz ""
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		# Stack pointer unchanged, no stack allocated arguments
20		movq $8, %rax
21		movq %rax, %rdi
22		call wacc_f
23		movq %rax, %r11
24		# Stack pointer unchanged, no stack allocated arguments
25		movq %r11, %rax
26		movq %rax, %r12
27		# Stack pointer unchanged, no stack allocated variables
28		movq $0, %rax
29		# popq {%rbx, %r12}
30		movq (%rsp), %rbx
31		movq 8(%rsp), %r12
32		addq $16, %rsp
33		popq %rbp
34		ret
35	
36	wacc_f:
37		pushq %rbp
38		# pushq {%r12, %r13}
39		subq $16, %rsp
40		movq %r12, (%rsp)
41		movq %r13, 8(%rsp)
42		movq %rsp, %rbp
43		cmpq $0, %rdi
44		je .L0
45		# Stack pointer unchanged, no stack allocated variables
46		movq %rdi, %rax
47		movq %rax, %r12
48		jmp .L2
49	.L3:
50		pushq %rdi
51		# Set up R11 as a temporary second base pointer for the caller saved things
52		movq %rsp, %r11
53		# Stack pointer unchanged, no stack allocated arguments
54		leaq .L.str0(%rip), %rax
55		pushq %rax
56		popq %rax
57		movq %rax, %rax
58		movq %rax, %rdi
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _prints
61		popq %rdi
62		movl %r12d, %eax
63		subl $1, %eax
64		jo _errOverflow
65		movslq %eax, %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %r12
70	.L2:
71		cmpq $0, %r12
72		jg .L3
73		pushq %rdi
74		# Set up R11 as a temporary second base pointer for the caller saved things
75		movq %rsp, %r11
76		# Stack pointer unchanged, no stack allocated arguments
77		leaq .L.str1(%rip), %rax
78		pushq %rax
79		popq %rax
80		movq %rax, %rax
81		movq %rax, %rdi
82		# statement primitives do not return results (but will clobber r0/rax)
83		call _prints
84		call _println
85		popq %rdi
86		pushq %rdi
87		# Set up R11 as a temporary second base pointer for the caller saved things
88		movq %rsp, %r11
89		# Stack pointer unchanged, no stack allocated arguments
90		movl %edi, %eax
91		subl $1, %eax
92		jo _errOverflow
93		movslq %eax, %rax
94		pushq %rax
95		popq %rax
96		movq %rax, %rax
97		movq %rax, %rdi
98		call wacc_f
99		movq %rax, %r11
100		# Stack pointer unchanged, no stack allocated arguments
101		popq %rdi
102		movq %r11, %rax
103		movq %rax, %r13
104		# Stack pointer unchanged, no stack allocated variables
105		jmp .L1
106	.L0:
107	.L1:
108		movq $0, %rax
109		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
110		movq %rbp, %rsp
111		# popq {%r12, %r13}
112		movq (%rsp), %r12
113		movq 8(%rsp), %r13
114		addq $16, %rsp
115		popq %rbp
116		ret
117		# 'ere be dragons: this is 100% dead code, functions always end in returns!
118	
119	.section .rodata
120	# length of .L._prints_str0
121		.int 4
122	.L._prints_str0:
123		.asciz "%.*s"
124	.text
125	_prints:
126		pushq %rbp
127		movq %rsp, %rbp
128		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
129		andq $-16, %rsp
130		movq %rdi, %rdx
131		movl -4(%rdi), %esi
132		leaq .L._prints_str0(%rip), %rdi
133		# on x86, al represents the number of SIMD registers used as variadic arguments
134		movb $0, %al
135		call printf@plt
136		movq $0, %rdi
137		call fflush@plt
138		movq %rbp, %rsp
139		popq %rbp
140		ret
141	
142	.section .rodata
143	# length of .L._println_str0
144		.int 0
145	.L._println_str0:
146		.asciz ""
147	.text
148	_println:
149		pushq %rbp
150		movq %rsp, %rbp
151		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
152		andq $-16, %rsp
153		leaq .L._println_str0(%rip), %rdi
154		call puts@plt
155		movq $0, %rdi
156		call fflush@plt
157		movq %rbp, %rsp
158		popq %rbp
159		ret
160	
161	_exit:
162		pushq %rbp
163		movq %rsp, %rbp
164		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
165		andq $-16, %rsp
166		call exit@plt
167		movq %rbp, %rsp
168		popq %rbp
169		ret
170	
171	.section .rodata
172	# length of .L._errOverflow_str0
173		.int 52
174	.L._errOverflow_str0:
175		.asciz "fatal error: integer overflow or underflow occurred\n"
176	.text
177	_errOverflow:
178		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
179		andq $-16, %rsp
180		leaq .L._errOverflow_str0(%rip), %rdi
181		call _prints
182		movb $-1, %dil
183		call exit@plt
===========================================================
-- Finished

