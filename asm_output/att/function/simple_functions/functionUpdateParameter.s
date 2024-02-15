./valid/function/simple_functions/functionUpdateParameter.wacc
calling the reference compiler on ./valid/function/simple_functions/functionUpdateParameter.wacc
-- Test: functionUpdateParameter.wacc

-- Uploaded file: 
---------------------------------------------------------------
# test that the passed parameter can be updated and used
# and that y remains the same

# Output:
# y is 1
# x is 1
# x is now 5
# y is still 1
#

# Program:

begin

  int f(int x) is
    print "x is ";
    println x;
    x = 5;
    print "x is now ";
    println x;
    return x
  end

  int y = 1;
  print "y is ";
  println y;
  int x = call f(y);
  print "y is still ";
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionUpdateParameter.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 5
4	.L.str0:
5		.asciz "x is "
6	# length of .L.str1
7		.int 9
8	.L.str1:
9		.asciz "x is now "
10	# length of .L.str2
11		.int 5
12	.L.str2:
13		.asciz "y is "
14	# length of .L.str3
15		.int 11
16	.L.str3:
17		.asciz "y is still "
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
28		movq $1, %rax
29		movq %rax, %r12
30		# Stack pointer unchanged, no stack allocated arguments
31		leaq .L.str2(%rip), %rax
32		pushq %rax
33		popq %rax
34		movq %rax, %rax
35		movq %rax, %rdi
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		# Stack pointer unchanged, no stack allocated arguments
39		movq %r12, %rax
40		movq %rax, %rdi
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _printi
43		call _println
44		# Stack pointer unchanged, no stack allocated arguments
45		movq %r12, %rax
46		movq %rax, %rdi
47		call wacc_f
48		movq %rax, %r11
49		# Stack pointer unchanged, no stack allocated arguments
50		movq %r11, %rax
51		movq %rax, %r13
52		# Stack pointer unchanged, no stack allocated arguments
53		leaq .L.str3(%rip), %rax
54		pushq %rax
55		popq %rax
56		movq %rax, %rax
57		movq %rax, %rdi
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _prints
60		# Stack pointer unchanged, no stack allocated arguments
61		movq %r12, %rax
62		movq %rax, %rdi
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _printi
65		call _println
66		# Stack pointer unchanged, no stack allocated variables
67		movq $0, %rax
68		# popq {%rbx, %r12, %r13}
69		movq (%rsp), %rbx
70		movq 8(%rsp), %r12
71		movq 16(%rsp), %r13
72		addq $24, %rsp
73		popq %rbp
74		ret
75	
76	wacc_f:
77		pushq %rbp
78		movq %rsp, %rbp
79		pushq %rdi
80		# Set up R11 as a temporary second base pointer for the caller saved things
81		movq %rsp, %r11
82		# Stack pointer unchanged, no stack allocated arguments
83		leaq .L.str0(%rip), %rax
84		pushq %rax
85		popq %rax
86		movq %rax, %rax
87		movq %rax, %rdi
88		# statement primitives do not return results (but will clobber r0/rax)
89		call _prints
90		popq %rdi
91		pushq %rdi
92		# Set up R11 as a temporary second base pointer for the caller saved things
93		movq %rsp, %r11
94		# Stack pointer unchanged, no stack allocated arguments
95		movq %rdi, %rax
96		movq %rax, %rdi
97		# statement primitives do not return results (but will clobber r0/rax)
98		call _printi
99		call _println
100		popq %rdi
101		movq $5, %rax
102		movq %rax, %rdi
103		pushq %rdi
104		# Set up R11 as a temporary second base pointer for the caller saved things
105		movq %rsp, %r11
106		# Stack pointer unchanged, no stack allocated arguments
107		leaq .L.str1(%rip), %rax
108		pushq %rax
109		popq %rax
110		movq %rax, %rax
111		movq %rax, %rdi
112		# statement primitives do not return results (but will clobber r0/rax)
113		call _prints
114		popq %rdi
115		pushq %rdi
116		# Set up R11 as a temporary second base pointer for the caller saved things
117		movq %rsp, %r11
118		# Stack pointer unchanged, no stack allocated arguments
119		movq %rdi, %rax
120		movq %rax, %rdi
121		# statement primitives do not return results (but will clobber r0/rax)
122		call _printi
123		call _println
124		popq %rdi
125		movq %rdi, %rax
126		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
127		movq %rbp, %rsp
128		popq %rbp
129		ret
130		# 'ere be dragons: this is 100% dead code, functions always end in returns!
131	
132	.section .rodata
133	# length of .L._prints_str0
134		.int 4
135	.L._prints_str0:
136		.asciz "%.*s"
137	.text
138	_prints:
139		pushq %rbp
140		movq %rsp, %rbp
141		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
142		andq $-16, %rsp
143		movq %rdi, %rdx
144		movl -4(%rdi), %esi
145		leaq .L._prints_str0(%rip), %rdi
146		# on x86, al represents the number of SIMD registers used as variadic arguments
147		movb $0, %al
148		call printf@plt
149		movq $0, %rdi
150		call fflush@plt
151		movq %rbp, %rsp
152		popq %rbp
153		ret
154	
155	.section .rodata
156	# length of .L._printi_str0
157		.int 2
158	.L._printi_str0:
159		.asciz "%d"
160	.text
161	_printi:
162		pushq %rbp
163		movq %rsp, %rbp
164		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
165		andq $-16, %rsp
166		movl %edi, %esi
167		leaq .L._printi_str0(%rip), %rdi
168		# on x86, al represents the number of SIMD registers used as variadic arguments
169		movb $0, %al
170		call printf@plt
171		movq $0, %rdi
172		call fflush@plt
173		movq %rbp, %rsp
174		popq %rbp
175		ret
176	
177	.section .rodata
178	# length of .L._println_str0
179		.int 0
180	.L._println_str0:
181		.asciz ""
182	.text
183	_println:
184		pushq %rbp
185		movq %rsp, %rbp
186		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
187		andq $-16, %rsp
188		leaq .L._println_str0(%rip), %rdi
189		call puts@plt
190		movq $0, %rdi
191		call fflush@plt
192		movq %rbp, %rsp
193		popq %rbp
194		ret
===========================================================
-- Finished

