./valid/while/rmStyleAddIO.wacc
calling the reference compiler on ./valid/while/rmStyleAddIO.wacc
-- Test: rmStyleAddIO.wacc

-- Uploaded file: 
---------------------------------------------------------------
# register machine style addition

# Input: 2 40

# Output:
# Enter the first number: Enter the second number: Initial value of x: 2
# (+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)(+)
# final value of x: 42
#

# Program:

begin
  int x = 0 ;
  int y = 0 ;
  print "Enter the first number: " ;
  read x ;
  print "Enter the second number: " ;
  read y ;
  print "Initial value of x: " ;
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
rmStyleAddIO.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 24
4	.L.str0:
5		.asciz "Enter the first number: "
6	# length of .L.str1
7		.int 25
8	.L.str1:
9		.asciz "Enter the second number: "
10	# length of .L.str2
11		.int 20
12	.L.str2:
13		.asciz "Initial value of x: "
14	# length of .L.str3
15		.int 3
16	.L.str3:
17		.asciz "(+)"
18	# length of .L.str4
19		.int 0
20	.L.str4:
21		.asciz ""
22	# length of .L.str5
23		.int 18
24	.L.str5:
25		.asciz "final value of x: "
26	.text
27	main:
28		pushq %rbp
29		# pushq {%rbx, %r12, %r13}
30		subq $24, %rsp
31		movq %rbx, (%rsp)
32		movq %r12, 8(%rsp)
33		movq %r13, 16(%rsp)
34		movq %rsp, %rbp
35		# Stack pointer unchanged, no stack allocated variables
36		movq $0, %rax
37		movq %rax, %r12
38		movq $0, %rax
39		movq %rax, %r13
40		# Stack pointer unchanged, no stack allocated arguments
41		leaq .L.str0(%rip), %rax
42		pushq %rax
43		popq %rax
44		movq %rax, %rax
45		movq %rax, %rdi
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _prints
48		# Stack pointer unchanged, no stack allocated arguments
49		# load the current value in the destination of the read so it supports defaults
50		movq %r12, %rax
51		movq %rax, %rdi
52		call _readi
53		movq %rax, %r11
54		movq %r11, %rax
55		movq %rax, %r12
56		# Stack pointer unchanged, no stack allocated arguments
57		leaq .L.str1(%rip), %rax
58		pushq %rax
59		popq %rax
60		movq %rax, %rax
61		movq %rax, %rdi
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _prints
64		# Stack pointer unchanged, no stack allocated arguments
65		# load the current value in the destination of the read so it supports defaults
66		movq %r13, %rax
67		movq %rax, %rdi
68		call _readi
69		movq %rax, %r11
70		movq %r11, %rax
71		movq %rax, %r13
72		# Stack pointer unchanged, no stack allocated arguments
73		leaq .L.str2(%rip), %rax
74		pushq %rax
75		popq %rax
76		movq %rax, %rax
77		movq %rax, %rdi
78		# statement primitives do not return results (but will clobber r0/rax)
79		call _prints
80		# Stack pointer unchanged, no stack allocated arguments
81		movq %r12, %rax
82		movq %rax, %rdi
83		# statement primitives do not return results (but will clobber r0/rax)
84		call _printi
85		call _println
86		jmp .L0
87	.L1:
88		# Stack pointer unchanged, no stack allocated arguments
89		leaq .L.str3(%rip), %rax
90		pushq %rax
91		popq %rax
92		movq %rax, %rax
93		movq %rax, %rdi
94		# statement primitives do not return results (but will clobber r0/rax)
95		call _prints
96		movl %r12d, %eax
97		addl $1, %eax
98		jo _errOverflow
99		movslq %eax, %rax
100		pushq %rax
101		popq %rax
102		movq %rax, %rax
103		movq %rax, %r12
104		movl %r13d, %eax
105		subl $1, %eax
106		jo _errOverflow
107		movslq %eax, %rax
108		pushq %rax
109		popq %rax
110		movq %rax, %rax
111		movq %rax, %r13
112	.L0:
113		cmpq $0, %r13
114		jg .L1
115		# Stack pointer unchanged, no stack allocated arguments
116		leaq .L.str4(%rip), %rax
117		pushq %rax
118		popq %rax
119		movq %rax, %rax
120		movq %rax, %rdi
121		# statement primitives do not return results (but will clobber r0/rax)
122		call _prints
123		call _println
124		# Stack pointer unchanged, no stack allocated arguments
125		leaq .L.str5(%rip), %rax
126		pushq %rax
127		popq %rax
128		movq %rax, %rax
129		movq %rax, %rdi
130		# statement primitives do not return results (but will clobber r0/rax)
131		call _prints
132		# Stack pointer unchanged, no stack allocated arguments
133		movq %r12, %rax
134		movq %rax, %rdi
135		# statement primitives do not return results (but will clobber r0/rax)
136		call _printi
137		call _println
138		# Stack pointer unchanged, no stack allocated variables
139		movq $0, %rax
140		# popq {%rbx, %r12, %r13}
141		movq (%rsp), %rbx
142		movq 8(%rsp), %r12
143		movq 16(%rsp), %r13
144		addq $24, %rsp
145		popq %rbp
146		ret
147	
148	.section .rodata
149	# length of .L._prints_str0
150		.int 4
151	.L._prints_str0:
152		.asciz "%.*s"
153	.text
154	_prints:
155		pushq %rbp
156		movq %rsp, %rbp
157		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
158		andq $-16, %rsp
159		movq %rdi, %rdx
160		movl -4(%rdi), %esi
161		leaq .L._prints_str0(%rip), %rdi
162		# on x86, al represents the number of SIMD registers used as variadic arguments
163		movb $0, %al
164		call printf@plt
165		movq $0, %rdi
166		call fflush@plt
167		movq %rbp, %rsp
168		popq %rbp
169		ret
170	
171	.section .rodata
172	# length of .L._readi_str0
173		.int 2
174	.L._readi_str0:
175		.asciz "%d"
176	.text
177	_readi:
178		pushq %rbp
179		movq %rsp, %rbp
180		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
181		andq $-16, %rsp
182		# RDI contains the "original" value of the destination of the read
183		# allocate space on the stack to store the read: preserve alignment!
184		# the passed default argument should be stored in case of EOF
185		subq $16, %rsp
186		movl %edi, (%rsp)
187		leaq (%rsp), %rsi
188		leaq .L._readi_str0(%rip), %rdi
189		# on x86, al represents the number of SIMD registers used as variadic arguments
190		movb $0, %al
191		call scanf@plt
192		movslq (%rsp), %rax
193		addq $16, %rsp
194		movq %rbp, %rsp
195		popq %rbp
196		ret
197	
198	.section .rodata
199	# length of .L._printi_str0
200		.int 2
201	.L._printi_str0:
202		.asciz "%d"
203	.text
204	_printi:
205		pushq %rbp
206		movq %rsp, %rbp
207		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
208		andq $-16, %rsp
209		movl %edi, %esi
210		leaq .L._printi_str0(%rip), %rdi
211		# on x86, al represents the number of SIMD registers used as variadic arguments
212		movb $0, %al
213		call printf@plt
214		movq $0, %rdi
215		call fflush@plt
216		movq %rbp, %rsp
217		popq %rbp
218		ret
219	
220	.section .rodata
221	# length of .L._println_str0
222		.int 0
223	.L._println_str0:
224		.asciz ""
225	.text
226	_println:
227		pushq %rbp
228		movq %rsp, %rbp
229		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
230		andq $-16, %rsp
231		leaq .L._println_str0(%rip), %rdi
232		call puts@plt
233		movq $0, %rdi
234		call fflush@plt
235		movq %rbp, %rsp
236		popq %rbp
237		ret
238	
239	_exit:
240		pushq %rbp
241		movq %rsp, %rbp
242		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
243		andq $-16, %rsp
244		call exit@plt
245		movq %rbp, %rsp
246		popq %rbp
247		ret
248	
249	.section .rodata
250	# length of .L._errOverflow_str0
251		.int 52
252	.L._errOverflow_str0:
253		.asciz "fatal error: integer overflow or underflow occurred\n"
254	.text
255	_errOverflow:
256		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
257		andq $-16, %rsp
258		leaq .L._errOverflow_str0(%rip), %rdi
259		call _prints
260		movb $-1, %dil
261		call exit@plt
===========================================================
-- Finished

