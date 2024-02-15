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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 2
5	.L.str0:
6		.asciz "Hi"
7	# length of .L.str1
8		.int 5
9	.L.str1:
10		.asciz "Hello"
11	# length of .L.str2
12		.int 6
13	.L.str2:
14		.asciz "s1 is "
15	# length of .L.str3
16		.int 6
17	.L.str3:
18		.asciz "s2 is "
19	# length of .L.str4
20		.int 29
21	.L.str4:
22		.asciz "They are not the same string."
23	# length of .L.str5
24		.int 25
25	.L.str5:
26		.asciz "They are the same string."
27	# length of .L.str6
28		.int 16
29	.L.str6:
30		.asciz "Now make s1 = s2"
31	.text
32	main:
33		push rbp
34		# push {rbx, r12, r13}
35		sub rsp, 24
36		mov qword ptr [rsp], rbx
37		mov qword ptr [rsp + 8], r12
38		mov qword ptr [rsp + 16], r13
39		mov rbp, rsp
40		# Stack pointer unchanged, no stack allocated variables
41		lea rax, [rip + .L.str0]
42		push rax
43		pop rax
44		mov rax, rax
45		mov r12, rax
46		lea rax, [rip + .L.str1]
47		push rax
48		pop rax
49		mov rax, rax
50		mov r13, rax
51		# Stack pointer unchanged, no stack allocated arguments
52		lea rax, [rip + .L.str2]
53		push rax
54		pop rax
55		mov rax, rax
56		mov rdi, rax
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _prints
59		# Stack pointer unchanged, no stack allocated arguments
60		mov rax, r12
61		mov rdi, rax
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _prints
64		call _println
65		# Stack pointer unchanged, no stack allocated arguments
66		lea rax, [rip + .L.str3]
67		push rax
68		pop rax
69		mov rax, rax
70		mov rdi, rax
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _prints
73		# Stack pointer unchanged, no stack allocated arguments
74		mov rax, r13
75		mov rdi, rax
76		# statement primitives do not return results (but will clobber r0/rax)
77		call _prints
78		call _println
79		cmp r12, r13
80		je .L0
81		# Stack pointer unchanged, no stack allocated arguments
82		lea rax, [rip + .L.str4]
83		push rax
84		pop rax
85		mov rax, rax
86		mov rdi, rax
87		# statement primitives do not return results (but will clobber r0/rax)
88		call _prints
89		call _println
90		jmp .L1
91	.L0:
92		# Stack pointer unchanged, no stack allocated arguments
93		lea rax, [rip + .L.str5]
94		push rax
95		pop rax
96		mov rax, rax
97		mov rdi, rax
98		# statement primitives do not return results (but will clobber r0/rax)
99		call _prints
100		call _println
101	.L1:
102		# Stack pointer unchanged, no stack allocated arguments
103		lea rax, [rip + .L.str6]
104		push rax
105		pop rax
106		mov rax, rax
107		mov rdi, rax
108		# statement primitives do not return results (but will clobber r0/rax)
109		call _prints
110		call _println
111		mov rax, r13
112		mov r12, rax
113		# Stack pointer unchanged, no stack allocated arguments
114		lea rax, [rip + .L.str2]
115		push rax
116		pop rax
117		mov rax, rax
118		mov rdi, rax
119		# statement primitives do not return results (but will clobber r0/rax)
120		call _prints
121		# Stack pointer unchanged, no stack allocated arguments
122		mov rax, r12
123		mov rdi, rax
124		# statement primitives do not return results (but will clobber r0/rax)
125		call _prints
126		call _println
127		# Stack pointer unchanged, no stack allocated arguments
128		lea rax, [rip + .L.str3]
129		push rax
130		pop rax
131		mov rax, rax
132		mov rdi, rax
133		# statement primitives do not return results (but will clobber r0/rax)
134		call _prints
135		# Stack pointer unchanged, no stack allocated arguments
136		mov rax, r13
137		mov rdi, rax
138		# statement primitives do not return results (but will clobber r0/rax)
139		call _prints
140		call _println
141		cmp r12, r13
142		je .L2
143		# Stack pointer unchanged, no stack allocated arguments
144		lea rax, [rip + .L.str4]
145		push rax
146		pop rax
147		mov rax, rax
148		mov rdi, rax
149		# statement primitives do not return results (but will clobber r0/rax)
150		call _prints
151		call _println
152		jmp .L3
153	.L2:
154		# Stack pointer unchanged, no stack allocated arguments
155		lea rax, [rip + .L.str5]
156		push rax
157		pop rax
158		mov rax, rax
159		mov rdi, rax
160		# statement primitives do not return results (but will clobber r0/rax)
161		call _prints
162		call _println
163	.L3:
164		# Stack pointer unchanged, no stack allocated variables
165		mov rax, 0
166		# pop {rbx, r12, r13}
167		mov rbx, qword ptr [rsp]
168		mov r12, qword ptr [rsp + 8]
169		mov r13, qword ptr [rsp + 16]
170		add rsp, 24
171		pop rbp
172		ret
173	
174	.section .rodata
175	# length of .L._prints_str0
176		.int 4
177	.L._prints_str0:
178		.asciz "%.*s"
179	.text
180	_prints:
181		push rbp
182		mov rbp, rsp
183		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
184		and rsp, -16
185		mov rdx, rdi
186		mov esi, dword ptr [rdi - 4]
187		lea rdi, [rip + .L._prints_str0]
188		# on x86, al represents the number of SIMD registers used as variadic arguments
189		mov al, 0
190		call printf@plt
191		mov rdi, 0
192		call fflush@plt
193		mov rsp, rbp
194		pop rbp
195		ret
196	
197	.section .rodata
198	# length of .L._println_str0
199		.int 0
200	.L._println_str0:
201		.asciz ""
202	.text
203	_println:
204		push rbp
205		mov rbp, rsp
206		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
207		and rsp, -16
208		lea rdi, [rip + .L._println_str0]
209		call puts@plt
210		mov rdi, 0
211		call fflush@plt
212		mov rsp, rbp
213		pop rbp
214		ret
===========================================================
-- Finished

