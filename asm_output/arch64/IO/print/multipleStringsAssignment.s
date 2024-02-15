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
0	.data
1	// length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz "Hi"
5	// length of .L.str1
6		.word 5
7	.L.str1:
8		.asciz "Hello"
9	// length of .L.str2
10		.word 6
11	.L.str2:
12		.asciz "s1 is "
13	// length of .L.str3
14		.word 6
15	.L.str3:
16		.asciz "s2 is "
17	// length of .L.str4
18		.word 29
19	.L.str4:
20		.asciz "They are not the same string."
21	// length of .L.str5
22		.word 25
23	.L.str5:
24		.asciz "They are the same string."
25	// length of .L.str6
26		.word 16
27	.L.str6:
28		.asciz "Now make s1 = s2"
29	.align 4
30	.text
31	.global main
32	main:
33		// push {fp, lr}
34		stp fp, lr, [sp, #-16]!
35		// push {x19, x20}
36		stp x19, x20, [sp, #-16]!
37		mov fp, sp
38		// Stack pointer unchanged, no stack allocated variables
39		adrp x8, .L.str0
40		add x8, x8, :lo12:.L.str0
41		// push {x8}
42		stp x8, xzr, [sp, #-16]!
43		// pop {x8}
44		ldp x8, xzr, [sp], #16
45		mov x8, x8
46		mov x19, x8
47		adrp x8, .L.str1
48		add x8, x8, :lo12:.L.str1
49		// push {x8}
50		stp x8, xzr, [sp, #-16]!
51		// pop {x8}
52		ldp x8, xzr, [sp], #16
53		mov x8, x8
54		mov x20, x8
55		// Stack pointer unchanged, no stack allocated arguments
56		adrp x8, .L.str2
57		add x8, x8, :lo12:.L.str2
58		// push {x8}
59		stp x8, xzr, [sp, #-16]!
60		// pop {x8}
61		ldp x8, xzr, [sp], #16
62		mov x8, x8
63		mov x0, x8
64		// statement primitives do not return results (but will clobber r0/rax)
65		bl _prints
66		// Stack pointer unchanged, no stack allocated arguments
67		mov x8, x19
68		mov x0, x8
69		// statement primitives do not return results (but will clobber r0/rax)
70		bl _prints
71		bl _println
72		// Stack pointer unchanged, no stack allocated arguments
73		adrp x8, .L.str3
74		add x8, x8, :lo12:.L.str3
75		// push {x8}
76		stp x8, xzr, [sp, #-16]!
77		// pop {x8}
78		ldp x8, xzr, [sp], #16
79		mov x8, x8
80		mov x0, x8
81		// statement primitives do not return results (but will clobber r0/rax)
82		bl _prints
83		// Stack pointer unchanged, no stack allocated arguments
84		mov x8, x20
85		mov x0, x8
86		// statement primitives do not return results (but will clobber r0/rax)
87		bl _prints
88		bl _println
89		cmp x19, x20
90		b.eq .L0
91		// Stack pointer unchanged, no stack allocated arguments
92		adrp x8, .L.str4
93		add x8, x8, :lo12:.L.str4
94		// push {x8}
95		stp x8, xzr, [sp, #-16]!
96		// pop {x8}
97		ldp x8, xzr, [sp], #16
98		mov x8, x8
99		mov x0, x8
100		// statement primitives do not return results (but will clobber r0/rax)
101		bl _prints
102		bl _println
103		b .L1
104	.L0:
105		// Stack pointer unchanged, no stack allocated arguments
106		adrp x8, .L.str5
107		add x8, x8, :lo12:.L.str5
108		// push {x8}
109		stp x8, xzr, [sp, #-16]!
110		// pop {x8}
111		ldp x8, xzr, [sp], #16
112		mov x8, x8
113		mov x0, x8
114		// statement primitives do not return results (but will clobber r0/rax)
115		bl _prints
116		bl _println
117	.L1:
118		// Stack pointer unchanged, no stack allocated arguments
119		adrp x8, .L.str6
120		add x8, x8, :lo12:.L.str6
121		// push {x8}
122		stp x8, xzr, [sp, #-16]!
123		// pop {x8}
124		ldp x8, xzr, [sp], #16
125		mov x8, x8
126		mov x0, x8
127		// statement primitives do not return results (but will clobber r0/rax)
128		bl _prints
129		bl _println
130		mov x8, x20
131		mov x19, x8
132		// Stack pointer unchanged, no stack allocated arguments
133		adrp x8, .L.str2
134		add x8, x8, :lo12:.L.str2
135		// push {x8}
136		stp x8, xzr, [sp, #-16]!
137		// pop {x8}
138		ldp x8, xzr, [sp], #16
139		mov x8, x8
140		mov x0, x8
141		// statement primitives do not return results (but will clobber r0/rax)
142		bl _prints
143		// Stack pointer unchanged, no stack allocated arguments
144		mov x8, x19
145		mov x0, x8
146		// statement primitives do not return results (but will clobber r0/rax)
147		bl _prints
148		bl _println
149		// Stack pointer unchanged, no stack allocated arguments
150		adrp x8, .L.str3
151		add x8, x8, :lo12:.L.str3
152		// push {x8}
153		stp x8, xzr, [sp, #-16]!
154		// pop {x8}
155		ldp x8, xzr, [sp], #16
156		mov x8, x8
157		mov x0, x8
158		// statement primitives do not return results (but will clobber r0/rax)
159		bl _prints
160		// Stack pointer unchanged, no stack allocated arguments
161		mov x8, x20
162		mov x0, x8
163		// statement primitives do not return results (but will clobber r0/rax)
164		bl _prints
165		bl _println
166		cmp x19, x20
167		b.eq .L2
168		// Stack pointer unchanged, no stack allocated arguments
169		adrp x8, .L.str4
170		add x8, x8, :lo12:.L.str4
171		// push {x8}
172		stp x8, xzr, [sp, #-16]!
173		// pop {x8}
174		ldp x8, xzr, [sp], #16
175		mov x8, x8
176		mov x0, x8
177		// statement primitives do not return results (but will clobber r0/rax)
178		bl _prints
179		bl _println
180		b .L3
181	.L2:
182		// Stack pointer unchanged, no stack allocated arguments
183		adrp x8, .L.str5
184		add x8, x8, :lo12:.L.str5
185		// push {x8}
186		stp x8, xzr, [sp, #-16]!
187		// pop {x8}
188		ldp x8, xzr, [sp], #16
189		mov x8, x8
190		mov x0, x8
191		// statement primitives do not return results (but will clobber r0/rax)
192		bl _prints
193		bl _println
194	.L3:
195		// Stack pointer unchanged, no stack allocated variables
196		mov x0, #0
197		// pop {x19, x20}
198		ldp x19, x20, [sp], #16
199		// pop {fp, lr}
200		ldp fp, lr, [sp], #16
201		ret
202	
203	// length of .L._prints_str0
204		.word 4
205	.L._prints_str0:
206		.asciz "%.*s"
207	.align 4
208	_prints:
209		// push {lr}
210		stp lr, xzr, [sp, #-16]!
211		mov x2, x0
212		ldrsw x1, [x0, #-4]
213		adr x0, .L._prints_str0
214		bl printf
215		mov x0, #0
216		bl fflush
217		// pop {lr}
218		ldp lr, xzr, [sp], #16
219		ret
220	
221	// length of .L._println_str0
222		.word 0
223	.L._println_str0:
224		.asciz ""
225	.align 4
226	_println:
227		// push {lr}
228		stp lr, xzr, [sp, #-16]!
229		adr x0, .L._println_str0
230		bl puts
231		mov x0, #0
232		bl fflush
233		// pop {lr}
234		ldp lr, xzr, [sp], #16
235		ret
===========================================================
-- Finished

