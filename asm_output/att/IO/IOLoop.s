./valid/IO/IOLoop.wacc
calling the reference compiler on ./valid/IO/IOLoop.wacc
-- Test: IOLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple input/output loop

# Input: 1 Y 2 Y 3 Y 4 Y 5 Y 142 N

# Output:
# Please input an integer: echo input: 1
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 2
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 3
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 4
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 5
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 142
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
#

# Program:

begin
  char continue = 'Y' ;
  int buff = 0 ;
  while continue != 'N' do
    print "Please input an integer: " ;
    read buff ;
    print "echo input: " ;
    println buff ;
    println "Do you want to continue entering input?" ;
    println "(enter Y for \'yes\' and N for \'no\')" ;
    read continue
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOLoop.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 25
4	.L.str0:
5		.asciz "Please input an integer: "
6	# length of .L.str1
7		.int 12
8	.L.str1:
9		.asciz "echo input: "
10	# length of .L.str2
11		.int 39
12	.L.str2:
13		.asciz "Do you want to continue entering input?"
14	# length of .L.str3
15		.int 34
16	.L.str3:
17		.asciz "(enter Y for 'yes' and N for 'no')"
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
28		movq $89, %rax
29		movq %rax, %r12
30		movq $0, %rax
31		movq %rax, %r13
32		jmp .L0
33	.L1:
34		# Stack pointer unchanged, no stack allocated arguments
35		leaq .L.str0(%rip), %rax
36		pushq %rax
37		popq %rax
38		movq %rax, %rax
39		movq %rax, %rdi
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _prints
42		# Stack pointer unchanged, no stack allocated arguments
43		# load the current value in the destination of the read so it supports defaults
44		movq %r13, %rax
45		movq %rax, %rdi
46		call _readi
47		movq %rax, %r11
48		movq %r11, %rax
49		movq %rax, %r13
50		# Stack pointer unchanged, no stack allocated arguments
51		leaq .L.str1(%rip), %rax
52		pushq %rax
53		popq %rax
54		movq %rax, %rax
55		movq %rax, %rdi
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _prints
58		# Stack pointer unchanged, no stack allocated arguments
59		movq %r13, %rax
60		movq %rax, %rdi
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _printi
63		call _println
64		# Stack pointer unchanged, no stack allocated arguments
65		leaq .L.str2(%rip), %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %rdi
70		# statement primitives do not return results (but will clobber r0/rax)
71		call _prints
72		call _println
73		# Stack pointer unchanged, no stack allocated arguments
74		leaq .L.str3(%rip), %rax
75		pushq %rax
76		popq %rax
77		movq %rax, %rax
78		movq %rax, %rdi
79		# statement primitives do not return results (but will clobber r0/rax)
80		call _prints
81		call _println
82		# Stack pointer unchanged, no stack allocated arguments
83		# load the current value in the destination of the read so it supports defaults
84		movq %r12, %rax
85		movq %rax, %rdi
86		call _readc
87		movq %rax, %r11
88		movq %r11, %rax
89		movq %rax, %r12
90	.L0:
91		cmpq $78, %r12
92		jne .L1
93		# Stack pointer unchanged, no stack allocated variables
94		movq $0, %rax
95		# popq {%rbx, %r12, %r13}
96		movq (%rsp), %rbx
97		movq 8(%rsp), %r12
98		movq 16(%rsp), %r13
99		addq $24, %rsp
100		popq %rbp
101		ret
102	
103	.section .rodata
104	# length of .L._readc_str0
105		.int 3
106	.L._readc_str0:
107		.asciz " %c"
108	.text
109	_readc:
110		pushq %rbp
111		movq %rsp, %rbp
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		andq $-16, %rsp
114		# RDI contains the "original" value of the destination of the read
115		# allocate space on the stack to store the read: preserve alignment!
116		# the passed default argument should be stored in case of EOF
117		subq $16, %rsp
118		movb %dil, (%rsp)
119		leaq (%rsp), %rsi
120		leaq .L._readc_str0(%rip), %rdi
121		# on x86, al represents the number of SIMD registers used as variadic arguments
122		movb $0, %al
123		call scanf@plt
124		movsbq (%rsp), %rax
125		addq $16, %rsp
126		movq %rbp, %rsp
127		popq %rbp
128		ret
129	
130	.section .rodata
131	# length of .L._prints_str0
132		.int 4
133	.L._prints_str0:
134		.asciz "%.*s"
135	.text
136	_prints:
137		pushq %rbp
138		movq %rsp, %rbp
139		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
140		andq $-16, %rsp
141		movq %rdi, %rdx
142		movl -4(%rdi), %esi
143		leaq .L._prints_str0(%rip), %rdi
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
154	# length of .L._readi_str0
155		.int 2
156	.L._readi_str0:
157		.asciz "%d"
158	.text
159	_readi:
160		pushq %rbp
161		movq %rsp, %rbp
162		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
163		andq $-16, %rsp
164		# RDI contains the "original" value of the destination of the read
165		# allocate space on the stack to store the read: preserve alignment!
166		# the passed default argument should be stored in case of EOF
167		subq $16, %rsp
168		movl %edi, (%rsp)
169		leaq (%rsp), %rsi
170		leaq .L._readi_str0(%rip), %rdi
171		# on x86, al represents the number of SIMD registers used as variadic arguments
172		movb $0, %al
173		call scanf@plt
174		movslq (%rsp), %rax
175		addq $16, %rsp
176		movq %rbp, %rsp
177		popq %rbp
178		ret
179	
180	.section .rodata
181	# length of .L._printi_str0
182		.int 2
183	.L._printi_str0:
184		.asciz "%d"
185	.text
186	_printi:
187		pushq %rbp
188		movq %rsp, %rbp
189		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
190		andq $-16, %rsp
191		movl %edi, %esi
192		leaq .L._printi_str0(%rip), %rdi
193		# on x86, al represents the number of SIMD registers used as variadic arguments
194		movb $0, %al
195		call printf@plt
196		movq $0, %rdi
197		call fflush@plt
198		movq %rbp, %rsp
199		popq %rbp
200		ret
201	
202	.section .rodata
203	# length of .L._println_str0
204		.int 0
205	.L._println_str0:
206		.asciz ""
207	.text
208	_println:
209		pushq %rbp
210		movq %rsp, %rbp
211		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
212		andq $-16, %rsp
213		leaq .L._println_str0(%rip), %rdi
214		call puts@plt
215		movq $0, %rdi
216		call fflush@plt
217		movq %rbp, %rsp
218		popq %rbp
219		ret
===========================================================
-- Finished

