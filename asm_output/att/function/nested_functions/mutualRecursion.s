./valid/function/nested_functions/mutualRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/mutualRecursion.wacc
-- Test: mutualRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a pair of mutually recursive functions

# Output:
# r1: sending 8
# r2: received 8
# r1: sending 7
# r2: received 7
# r1: sending 6
# r2: received 6
# r1: sending 5
# r2: received 5
# r1: sending 4
# r2: received 4
# r1: sending 3
# r2: received 3
# r1: sending 2
# r2: received 2
# r1: sending 1
# r2: received 1
#

# Program:

begin
  int r1(int x) is
    if x == 0
    then
      skip
    else
      print "r1: sending " ;
      println x ;
      int y = call r2(x)
    fi ;
    return 42
  end

  int r2(int y) is
    print "r2: received " ;
    println y ;
    int z = call r1(y - 1) ;
    return 44
  end

  int x = 0 ;
  x = call r1(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
mutualRecursion.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 12
4	.L.str0:
5		.asciz "r1: sending "
6	# length of .L.str1
7		.int 13
8	.L.str1:
9		.asciz "r2: received "
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $0, %rax
20		movq %rax, %r12
21		# Stack pointer unchanged, no stack allocated arguments
22		movq $8, %rax
23		movq %rax, %rdi
24		call wacc_r1
25		movq %rax, %r11
26		# Stack pointer unchanged, no stack allocated arguments
27		movq %r11, %rax
28		movq %rax, %r12
29		# Stack pointer unchanged, no stack allocated variables
30		movq $0, %rax
31		# popq {%rbx, %r12}
32		movq (%rsp), %rbx
33		movq 8(%rsp), %r12
34		addq $16, %rsp
35		popq %rbp
36		ret
37	
38	wacc_r1:
39		pushq %rbp
40		pushq %r12
41		movq %rsp, %rbp
42		cmpq $0, %rdi
43		je .L0
44		# Stack pointer unchanged, no stack allocated variables
45		pushq %rdi
46		# Set up R11 as a temporary second base pointer for the caller saved things
47		movq %rsp, %r11
48		# Stack pointer unchanged, no stack allocated arguments
49		leaq .L.str0(%rip), %rax
50		pushq %rax
51		popq %rax
52		movq %rax, %rax
53		movq %rax, %rdi
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _prints
56		popq %rdi
57		pushq %rdi
58		# Set up R11 as a temporary second base pointer for the caller saved things
59		movq %rsp, %r11
60		# Stack pointer unchanged, no stack allocated arguments
61		movq %rdi, %rax
62		movq %rax, %rdi
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _printi
65		call _println
66		popq %rdi
67		pushq %rdi
68		# Set up R11 as a temporary second base pointer for the caller saved things
69		movq %rsp, %r11
70		# Stack pointer unchanged, no stack allocated arguments
71		movq %rdi, %rax
72		movq %rax, %rdi
73		call wacc_r2
74		movq %rax, %r11
75		# Stack pointer unchanged, no stack allocated arguments
76		popq %rdi
77		movq %r11, %rax
78		movq %rax, %r12
79		# Stack pointer unchanged, no stack allocated variables
80		jmp .L1
81	.L0:
82	.L1:
83		movq $42, %rax
84		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
85		movq %rbp, %rsp
86		popq %r12
87		popq %rbp
88		ret
89		# 'ere be dragons: this is 100% dead code, functions always end in returns!
90	
91	wacc_r2:
92		pushq %rbp
93		pushq %r12
94		movq %rsp, %rbp
95		# Stack pointer unchanged, no stack allocated variables
96		pushq %rdi
97		# Set up R11 as a temporary second base pointer for the caller saved things
98		movq %rsp, %r11
99		# Stack pointer unchanged, no stack allocated arguments
100		leaq .L.str1(%rip), %rax
101		pushq %rax
102		popq %rax
103		movq %rax, %rax
104		movq %rax, %rdi
105		# statement primitives do not return results (but will clobber r0/rax)
106		call _prints
107		popq %rdi
108		pushq %rdi
109		# Set up R11 as a temporary second base pointer for the caller saved things
110		movq %rsp, %r11
111		# Stack pointer unchanged, no stack allocated arguments
112		movq %rdi, %rax
113		movq %rax, %rdi
114		# statement primitives do not return results (but will clobber r0/rax)
115		call _printi
116		call _println
117		popq %rdi
118		pushq %rdi
119		# Set up R11 as a temporary second base pointer for the caller saved things
120		movq %rsp, %r11
121		# Stack pointer unchanged, no stack allocated arguments
122		movl %edi, %eax
123		subl $1, %eax
124		jo _errOverflow
125		movslq %eax, %rax
126		pushq %rax
127		popq %rax
128		movq %rax, %rax
129		movq %rax, %rdi
130		call wacc_r1
131		movq %rax, %r11
132		# Stack pointer unchanged, no stack allocated arguments
133		popq %rdi
134		movq %r11, %rax
135		movq %rax, %r12
136		movq $44, %rax
137		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
138		movq %rbp, %rsp
139		popq %r12
140		popq %rbp
141		ret
142		# Stack pointer unchanged, no stack allocated variables
143		# 'ere be dragons: this is 100% dead code, functions always end in returns!
144	
145	.section .rodata
146	# length of .L._prints_str0
147		.int 4
148	.L._prints_str0:
149		.asciz "%.*s"
150	.text
151	_prints:
152		pushq %rbp
153		movq %rsp, %rbp
154		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
155		andq $-16, %rsp
156		movq %rdi, %rdx
157		movl -4(%rdi), %esi
158		leaq .L._prints_str0(%rip), %rdi
159		# on x86, al represents the number of SIMD registers used as variadic arguments
160		movb $0, %al
161		call printf@plt
162		movq $0, %rdi
163		call fflush@plt
164		movq %rbp, %rsp
165		popq %rbp
166		ret
167	
168	.section .rodata
169	# length of .L._printi_str0
170		.int 2
171	.L._printi_str0:
172		.asciz "%d"
173	.text
174	_printi:
175		pushq %rbp
176		movq %rsp, %rbp
177		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
178		andq $-16, %rsp
179		movl %edi, %esi
180		leaq .L._printi_str0(%rip), %rdi
181		# on x86, al represents the number of SIMD registers used as variadic arguments
182		movb $0, %al
183		call printf@plt
184		movq $0, %rdi
185		call fflush@plt
186		movq %rbp, %rsp
187		popq %rbp
188		ret
189	
190	.section .rodata
191	# length of .L._println_str0
192		.int 0
193	.L._println_str0:
194		.asciz ""
195	.text
196	_println:
197		pushq %rbp
198		movq %rsp, %rbp
199		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
200		andq $-16, %rsp
201		leaq .L._println_str0(%rip), %rdi
202		call puts@plt
203		movq $0, %rdi
204		call fflush@plt
205		movq %rbp, %rsp
206		popq %rbp
207		ret
208	
209	_exit:
210		pushq %rbp
211		movq %rsp, %rbp
212		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
213		andq $-16, %rsp
214		call exit@plt
215		movq %rbp, %rsp
216		popq %rbp
217		ret
218	
219	.section .rodata
220	# length of .L._errOverflow_str0
221		.int 52
222	.L._errOverflow_str0:
223		.asciz "fatal error: integer overflow or underflow occurred\n"
224	.text
225	_errOverflow:
226		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
227		andq $-16, %rsp
228		leaq .L._errOverflow_str0(%rip), %rdi
229		call _prints
230		movb $-1, %dil
231		call exit@plt
===========================================================
-- Finished

