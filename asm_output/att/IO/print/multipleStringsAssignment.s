./valid/IO/print/multipleStringsAssignment.wacc
calling the reference compiler on ./valid/IO/print/multipleStringsAssignment.wacc
-- Test: multipleStringsAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiple string assignments and comparisons

# Output:
# s1 is Hi
# s2 is Hello
# They are not the same string.
# Now make s1 = s2
# s1 is Hello
# s2 is Hello
# They are the same string.
#

# Program:

begin
  string s1 = "Hi" ;
  string s2 = "Hello" ;
  print "s1 is " ;
  println s1 ;
  print "s2 is " ;
  println s2 ;
  if s1 == s2 then
    println "They are the same string."
  else
    println "They are not the same string."
  fi ;

  println "Now make s1 = s2" ;
  s1 = s2 ;

  print "s1 is " ;
  println s1 ;
  print "s2 is " ;
  println s2 ;
  if s1 == s2 then
    println "They are the same string."
  else
    println "They are not the same string."
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
multipleStringsAssignment.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 2
4	.L.str0:
5		.asciz "Hi"
6	# length of .L.str1
7		.int 5
8	.L.str1:
9		.asciz "Hello"
10	# length of .L.str2
11		.int 6
12	.L.str2:
13		.asciz "s1 is "
14	# length of .L.str3
15		.int 6
16	.L.str3:
17		.asciz "s2 is "
18	# length of .L.str4
19		.int 29
20	.L.str4:
21		.asciz "They are not the same string."
22	# length of .L.str5
23		.int 25
24	.L.str5:
25		.asciz "They are the same string."
26	# length of .L.str6
27		.int 16
28	.L.str6:
29		.asciz "Now make s1 = s2"
30	.text
31	main:
32		pushq %rbp
33		# pushq {%rbx, %r12, %r13}
34		subq $24, %rsp
35		movq %rbx, (%rsp)
36		movq %r12, 8(%rsp)
37		movq %r13, 16(%rsp)
38		movq %rsp, %rbp
39		# Stack pointer unchanged, no stack allocated variables
40		leaq .L.str0(%rip), %rax
41		pushq %rax
42		popq %rax
43		movq %rax, %rax
44		movq %rax, %r12
45		leaq .L.str1(%rip), %rax
46		pushq %rax
47		popq %rax
48		movq %rax, %rax
49		movq %rax, %r13
50		# Stack pointer unchanged, no stack allocated arguments
51		leaq .L.str2(%rip), %rax
52		pushq %rax
53		popq %rax
54		movq %rax, %rax
55		movq %rax, %rdi
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _prints
58		# Stack pointer unchanged, no stack allocated arguments
59		movq %r12, %rax
60		movq %rax, %rdi
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _prints
63		call _println
64		# Stack pointer unchanged, no stack allocated arguments
65		leaq .L.str3(%rip), %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %rdi
70		# statement primitives do not return results (but will clobber r0/rax)
71		call _prints
72		# Stack pointer unchanged, no stack allocated arguments
73		movq %r13, %rax
74		movq %rax, %rdi
75		# statement primitives do not return results (but will clobber r0/rax)
76		call _prints
77		call _println
78		cmpq %r13, %r12
79		je .L0
80		# Stack pointer unchanged, no stack allocated arguments
81		leaq .L.str4(%rip), %rax
82		pushq %rax
83		popq %rax
84		movq %rax, %rax
85		movq %rax, %rdi
86		# statement primitives do not return results (but will clobber r0/rax)
87		call _prints
88		call _println
89		jmp .L1
90	.L0:
91		# Stack pointer unchanged, no stack allocated arguments
92		leaq .L.str5(%rip), %rax
93		pushq %rax
94		popq %rax
95		movq %rax, %rax
96		movq %rax, %rdi
97		# statement primitives do not return results (but will clobber r0/rax)
98		call _prints
99		call _println
100	.L1:
101		# Stack pointer unchanged, no stack allocated arguments
102		leaq .L.str6(%rip), %rax
103		pushq %rax
104		popq %rax
105		movq %rax, %rax
106		movq %rax, %rdi
107		# statement primitives do not return results (but will clobber r0/rax)
108		call _prints
109		call _println
110		movq %r13, %rax
111		movq %rax, %r12
112		# Stack pointer unchanged, no stack allocated arguments
113		leaq .L.str2(%rip), %rax
114		pushq %rax
115		popq %rax
116		movq %rax, %rax
117		movq %rax, %rdi
118		# statement primitives do not return results (but will clobber r0/rax)
119		call _prints
120		# Stack pointer unchanged, no stack allocated arguments
121		movq %r12, %rax
122		movq %rax, %rdi
123		# statement primitives do not return results (but will clobber r0/rax)
124		call _prints
125		call _println
126		# Stack pointer unchanged, no stack allocated arguments
127		leaq .L.str3(%rip), %rax
128		pushq %rax
129		popq %rax
130		movq %rax, %rax
131		movq %rax, %rdi
132		# statement primitives do not return results (but will clobber r0/rax)
133		call _prints
134		# Stack pointer unchanged, no stack allocated arguments
135		movq %r13, %rax
136		movq %rax, %rdi
137		# statement primitives do not return results (but will clobber r0/rax)
138		call _prints
139		call _println
140		cmpq %r13, %r12
141		je .L2
142		# Stack pointer unchanged, no stack allocated arguments
143		leaq .L.str4(%rip), %rax
144		pushq %rax
145		popq %rax
146		movq %rax, %rax
147		movq %rax, %rdi
148		# statement primitives do not return results (but will clobber r0/rax)
149		call _prints
150		call _println
151		jmp .L3
152	.L2:
153		# Stack pointer unchanged, no stack allocated arguments
154		leaq .L.str5(%rip), %rax
155		pushq %rax
156		popq %rax
157		movq %rax, %rax
158		movq %rax, %rdi
159		# statement primitives do not return results (but will clobber r0/rax)
160		call _prints
161		call _println
162	.L3:
163		# Stack pointer unchanged, no stack allocated variables
164		movq $0, %rax
165		# popq {%rbx, %r12, %r13}
166		movq (%rsp), %rbx
167		movq 8(%rsp), %r12
168		movq 16(%rsp), %r13
169		addq $24, %rsp
170		popq %rbp
171		ret
172	
173	.section .rodata
174	# length of .L._prints_str0
175		.int 4
176	.L._prints_str0:
177		.asciz "%.*s"
178	.text
179	_prints:
180		pushq %rbp
181		movq %rsp, %rbp
182		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
183		andq $-16, %rsp
184		movq %rdi, %rdx
185		movl -4(%rdi), %esi
186		leaq .L._prints_str0(%rip), %rdi
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
===========================================================
-- Finished

