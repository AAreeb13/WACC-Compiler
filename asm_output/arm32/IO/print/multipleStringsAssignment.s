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
1	@ length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz "Hi"
5	@ length of .L.str1
6		.word 5
7	.L.str1:
8		.asciz "Hello"
9	@ length of .L.str2
10		.word 6
11	.L.str2:
12		.asciz "s1 is "
13	@ length of .L.str3
14		.word 6
15	.L.str3:
16		.asciz "s2 is "
17	@ length of .L.str4
18		.word 29
19	.L.str4:
20		.asciz "They are not the same string."
21	@ length of .L.str5
22		.word 25
23	.L.str5:
24		.asciz "They are the same string."
25	@ length of .L.str6
26		.word 16
27	.L.str6:
28		.asciz "Now make s1 = s2"
29	.align 4
30	.text
31	.global main
32	main:
33		push {fp, lr}
34		push {r4, r5, r8, r10, r12}
35		mov fp, sp
36		@ Stack pointer unchanged, no stack allocated variables
37		ldr r8, =.L.str0
38		push {r8}
39		pop {r8}
40		mov r8, r8
41		mov r4, r8
42		ldr r8, =.L.str1
43		push {r8}
44		pop {r8}
45		mov r8, r8
46		mov r5, r8
47		@ Stack pointer unchanged, no stack allocated arguments
48		ldr r8, =.L.str2
49		push {r8}
50		pop {r8}
51		mov r8, r8
52		mov r0, r8
53		@ statement primitives do not return results (but will clobber r0/rax)
54		bl _prints
55		@ Stack pointer unchanged, no stack allocated arguments
56		mov r8, r4
57		mov r0, r8
58		@ statement primitives do not return results (but will clobber r0/rax)
59		bl _prints
60		bl _println
61		@ Stack pointer unchanged, no stack allocated arguments
62		ldr r8, =.L.str3
63		push {r8}
64		pop {r8}
65		mov r8, r8
66		mov r0, r8
67		@ statement primitives do not return results (but will clobber r0/rax)
68		bl _prints
69		@ Stack pointer unchanged, no stack allocated arguments
70		mov r8, r5
71		mov r0, r8
72		@ statement primitives do not return results (but will clobber r0/rax)
73		bl _prints
74		bl _println
75		cmp r4, r5
76		beq .L0
77		@ Stack pointer unchanged, no stack allocated arguments
78		ldr r8, =.L.str4
79		push {r8}
80		pop {r8}
81		mov r8, r8
82		mov r0, r8
83		@ statement primitives do not return results (but will clobber r0/rax)
84		bl _prints
85		bl _println
86		b .L1
87	.L0:
88		@ Stack pointer unchanged, no stack allocated arguments
89		ldr r8, =.L.str5
90		push {r8}
91		pop {r8}
92		mov r8, r8
93		mov r0, r8
94		@ statement primitives do not return results (but will clobber r0/rax)
95		bl _prints
96		bl _println
97	.L1:
98		@ Stack pointer unchanged, no stack allocated arguments
99		ldr r8, =.L.str6
100		push {r8}
101		pop {r8}
102		mov r8, r8
103		mov r0, r8
104		@ statement primitives do not return results (but will clobber r0/rax)
105		bl _prints
106		bl _println
107		mov r8, r5
108		mov r4, r8
109		@ Stack pointer unchanged, no stack allocated arguments
110		ldr r8, =.L.str2
111		push {r8}
112		pop {r8}
113		mov r8, r8
114		mov r0, r8
115		@ statement primitives do not return results (but will clobber r0/rax)
116		bl _prints
117		@ Stack pointer unchanged, no stack allocated arguments
118		mov r8, r4
119		mov r0, r8
120		@ statement primitives do not return results (but will clobber r0/rax)
121		bl _prints
122		bl _println
123		@ Stack pointer unchanged, no stack allocated arguments
124		ldr r8, =.L.str3
125		push {r8}
126		pop {r8}
127		mov r8, r8
128		mov r0, r8
129		@ statement primitives do not return results (but will clobber r0/rax)
130		bl _prints
131		@ Stack pointer unchanged, no stack allocated arguments
132		mov r8, r5
133		mov r0, r8
134		@ statement primitives do not return results (but will clobber r0/rax)
135		bl _prints
136		bl _println
137		cmp r4, r5
138		beq .L2
139		@ Stack pointer unchanged, no stack allocated arguments
140		ldr r8, =.L.str4
141		push {r8}
142		pop {r8}
143		mov r8, r8
144		mov r0, r8
145		@ statement primitives do not return results (but will clobber r0/rax)
146		bl _prints
147		bl _println
148		b .L3
149	.L2:
150		@ Stack pointer unchanged, no stack allocated arguments
151		ldr r8, =.L.str5
152		push {r8}
153		pop {r8}
154		mov r8, r8
155		mov r0, r8
156		@ statement primitives do not return results (but will clobber r0/rax)
157		bl _prints
158		bl _println
159	.L3:
160		@ Stack pointer unchanged, no stack allocated variables
161		mov r0, #0
162		pop {r4, r5, r8, r10, r12}
163		pop {fp, pc}
164	
165	@ length of .L._prints_str0
166		.word 4
167	.L._prints_str0:
168		.asciz "%.*s"
169	.align 4
170	_prints:
171		push {fp, lr}
172		mov fp, sp
173		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
174		bic sp, sp, #0x7
175		mov r2, r0
176		ldr r1, [r0, #-4]
177		adr r0, .L._prints_str0
178		bl printf
179		mov r0, #0
180		bl fflush
181		mov sp, fp
182		pop {fp, pc}
183	
184	@ length of .L._println_str0
185		.word 0
186	.L._println_str0:
187		.asciz ""
188	.align 4
189	_println:
190		push {fp, lr}
191		mov fp, sp
192		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
193		bic sp, sp, #0x7
194		adr r0, .L._println_str0
195		bl puts
196		mov r0, #0
197		bl fflush
198		mov sp, fp
199		pop {fp, pc}
===========================================================
-- Finished

