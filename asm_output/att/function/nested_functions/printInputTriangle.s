./valid/function/nested_functions/printInputTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printInputTriangle.wacc
-- Test: printInputTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a user-specified sized triangle

# Input: 13

# Output:
# Please enter the size of the triangle to print:
# -------------
# ------------
# -----------
# ----------
# ---------
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

  println "Please enter the size of the triangle to print:" ;
  int x = 0;

  read x;
  int s = call f(x)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printInputTriangle.s contents are:
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
10	# length of .L.str2
11		.int 47
12	.L.str2:
13		.asciz "Please enter the size of the triangle to print:"
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12, %r13}
18		subq $24, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %rsp, %rbp
23		# Stack pointer unchanged, no stack allocated variables
24		# Stack pointer unchanged, no stack allocated arguments
25		leaq .L.str2(%rip), %rax
26		pushq %rax
27		popq %rax
28		movq %rax, %rax
29		movq %rax, %rdi
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _prints
32		call _println
33		movq $0, %rax
34		movq %rax, %r12
35		# Stack pointer unchanged, no stack allocated arguments
36		# load the current value in the destination of the read so it supports defaults
37		movq %r12, %rax
38		movq %rax, %rdi
39		call _readi
40		movq %rax, %r11
41		movq %r11, %rax
42		movq %rax, %r12
43		# Stack pointer unchanged, no stack allocated arguments
44		movq %r12, %rax
45		movq %rax, %rdi
46		call wacc_f
47		movq %rax, %r11
48		# Stack pointer unchanged, no stack allocated arguments
49		movq %r11, %rax
50		movq %rax, %r13
51		# Stack pointer unchanged, no stack allocated variables
52		movq $0, %rax
53		# popq {%rbx, %r12, %r13}
54		movq (%rsp), %rbx
55		movq 8(%rsp), %r12
56		movq 16(%rsp), %r13
57		addq $24, %rsp
58		popq %rbp
59		ret
60	
61	wacc_f:
62		pushq %rbp
63		# pushq {%r12, %r13}
64		subq $16, %rsp
65		movq %r12, (%rsp)
66		movq %r13, 8(%rsp)
67		movq %rsp, %rbp
68		cmpq $0, %rdi
69		je .L0
70		# Stack pointer unchanged, no stack allocated variables
71		movq %rdi, %rax
72		movq %rax, %r12
73		jmp .L2
74	.L3:
75		pushq %rdi
76		# Set up R11 as a temporary second base pointer for the caller saved things
77		movq %rsp, %r11
78		# Stack pointer unchanged, no stack allocated arguments
79		leaq .L.str0(%rip), %rax
80		pushq %rax
81		popq %rax
82		movq %rax, %rax
83		movq %rax, %rdi
84		# statement primitives do not return results (but will clobber r0/rax)
85		call _prints
86		popq %rdi
87		movl %r12d, %eax
88		subl $1, %eax
89		jo _errOverflow
90		movslq %eax, %rax
91		pushq %rax
92		popq %rax
93		movq %rax, %rax
94		movq %rax, %r12
95	.L2:
96		cmpq $0, %r12
97		jg .L3
98		pushq %rdi
99		# Set up R11 as a temporary second base pointer for the caller saved things
100		movq %rsp, %r11
101		# Stack pointer unchanged, no stack allocated arguments
102		leaq .L.str1(%rip), %rax
103		pushq %rax
104		popq %rax
105		movq %rax, %rax
106		movq %rax, %rdi
107		# statement primitives do not return results (but will clobber r0/rax)
108		call _prints
109		call _println
110		popq %rdi
111		pushq %rdi
112		# Set up R11 as a temporary second base pointer for the caller saved things
113		movq %rsp, %r11
114		# Stack pointer unchanged, no stack allocated arguments
115		movl %edi, %eax
116		subl $1, %eax
117		jo _errOverflow
118		movslq %eax, %rax
119		pushq %rax
120		popq %rax
121		movq %rax, %rax
122		movq %rax, %rdi
123		call wacc_f
124		movq %rax, %r11
125		# Stack pointer unchanged, no stack allocated arguments
126		popq %rdi
127		movq %r11, %rax
128		movq %rax, %r13
129		# Stack pointer unchanged, no stack allocated variables
130		jmp .L1
131	.L0:
132	.L1:
133		movq $0, %rax
134		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
135		movq %rbp, %rsp
136		# popq {%r12, %r13}
137		movq (%rsp), %r12
138		movq 8(%rsp), %r13
139		addq $16, %rsp
140		popq %rbp
141		ret
142		# 'ere be dragons: this is 100% dead code, functions always end in returns!
143	
144	.section .rodata
145	# length of .L._prints_str0
146		.int 4
147	.L._prints_str0:
148		.asciz "%.*s"
149	.text
150	_prints:
151		pushq %rbp
152		movq %rsp, %rbp
153		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
154		andq $-16, %rsp
155		movq %rdi, %rdx
156		movl -4(%rdi), %esi
157		leaq .L._prints_str0(%rip), %rdi
158		# on x86, al represents the number of SIMD registers used as variadic arguments
159		movb $0, %al
160		call printf@plt
161		movq $0, %rdi
162		call fflush@plt
163		movq %rbp, %rsp
164		popq %rbp
165		ret
166	
167	.section .rodata
168	# length of .L._readi_str0
169		.int 2
170	.L._readi_str0:
171		.asciz "%d"
172	.text
173	_readi:
174		pushq %rbp
175		movq %rsp, %rbp
176		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
177		andq $-16, %rsp
178		# RDI contains the "original" value of the destination of the read
179		# allocate space on the stack to store the read: preserve alignment!
180		# the passed default argument should be stored in case of EOF
181		subq $16, %rsp
182		movl %edi, (%rsp)
183		leaq (%rsp), %rsi
184		leaq .L._readi_str0(%rip), %rdi
185		# on x86, al represents the number of SIMD registers used as variadic arguments
186		movb $0, %al
187		call scanf@plt
188		movslq (%rsp), %rax
189		addq $16, %rsp
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
224	# length of .L._errOverflow_str0
225		.int 52
226	.L._errOverflow_str0:
227		.asciz "fatal error: integer overflow or underflow occurred\n"
228	.text
229	_errOverflow:
230		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
231		andq $-16, %rsp
232		leaq .L._errOverflow_str0(%rip), %rdi
233		call _prints
234		movb $-1, %dil
235		call exit@plt
===========================================================
-- Finished

