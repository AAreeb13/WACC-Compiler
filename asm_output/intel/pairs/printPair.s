./valid/pairs/printPair.wacc
calling the reference compiler on ./valid/pairs/printPair.wacc
-- Test: printPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair program

# Output:
# #addrs# = (10, a)
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  print p ;
  print " = (" ;
  int x = fst p ;
  print x ;
  print ", " ;
  char c = snd p ;
  print c ;
  println ')'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPair.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 4
5	.L.str0:
6		.asciz " = ("
7	# length of .L.str1
8		.int 2
9	.L.str1:
10		.asciz ", "
11	.text
12	main:
13		push rbp
14		# push {rbx, r12, r13, r14}
15		sub rsp, 32
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov qword ptr [rsp + 16], r13
19		mov qword ptr [rsp + 24], r14
20		mov rbp, rsp
21		# Stack pointer unchanged, no stack allocated variables
22		mov edi, 16
23		call _malloc
24		mov r11, rax
25		mov rax, 10
26		mov qword ptr [r11], rax
27		mov rax, 97
28		mov qword ptr [r11 + 8], rax
29		mov rax, r11
30		mov r12, rax
31		# Stack pointer unchanged, no stack allocated arguments
32		mov rax, r12
33		mov rdi, rax
34		# statement primitives do not return results (but will clobber r0/rax)
35		call _printp
36		# Stack pointer unchanged, no stack allocated arguments
37		lea rax, [rip + .L.str0]
38		push rax
39		pop rax
40		mov rax, rax
41		mov rdi, rax
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _prints
44		cmp r12, 0
45		je _errNull
46		mov rax, qword ptr [r12]
47		mov rax, rax
48		mov r13, rax
49		# Stack pointer unchanged, no stack allocated arguments
50		mov rax, r13
51		mov rdi, rax
52		# statement primitives do not return results (but will clobber r0/rax)
53		call _printi
54		# Stack pointer unchanged, no stack allocated arguments
55		lea rax, [rip + .L.str1]
56		push rax
57		pop rax
58		mov rax, rax
59		mov rdi, rax
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _prints
62		cmp r12, 0
63		je _errNull
64		mov rax, qword ptr [r12 + 8]
65		mov rax, rax
66		mov r14, rax
67		# Stack pointer unchanged, no stack allocated arguments
68		mov rax, r14
69		mov rdi, rax
70		# statement primitives do not return results (but will clobber r0/rax)
71		call _printc
72		# Stack pointer unchanged, no stack allocated arguments
73		mov rax, 41
74		mov rdi, rax
75		# statement primitives do not return results (but will clobber r0/rax)
76		call _printc
77		call _println
78		# Stack pointer unchanged, no stack allocated variables
79		mov rax, 0
80		# pop {rbx, r12, r13, r14}
81		mov rbx, qword ptr [rsp]
82		mov r12, qword ptr [rsp + 8]
83		mov r13, qword ptr [rsp + 16]
84		mov r14, qword ptr [rsp + 24]
85		add rsp, 32
86		pop rbp
87		ret
88	
89	.section .rodata
90	# length of .L._prints_str0
91		.int 4
92	.L._prints_str0:
93		.asciz "%.*s"
94	.text
95	_prints:
96		push rbp
97		mov rbp, rsp
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		and rsp, -16
100		mov rdx, rdi
101		mov esi, dword ptr [rdi - 4]
102		lea rdi, [rip + .L._prints_str0]
103		# on x86, al represents the number of SIMD registers used as variadic arguments
104		mov al, 0
105		call printf@plt
106		mov rdi, 0
107		call fflush@plt
108		mov rsp, rbp
109		pop rbp
110		ret
111	
112	.section .rodata
113	# length of .L._printp_str0
114		.int 2
115	.L._printp_str0:
116		.asciz "%p"
117	.text
118	_printp:
119		push rbp
120		mov rbp, rsp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		and rsp, -16
123		mov rsi, rdi
124		lea rdi, [rip + .L._printp_str0]
125		# on x86, al represents the number of SIMD registers used as variadic arguments
126		mov al, 0
127		call printf@plt
128		mov rdi, 0
129		call fflush@plt
130		mov rsp, rbp
131		pop rbp
132		ret
133	
134	_malloc:
135		push rbp
136		mov rbp, rsp
137		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
138		and rsp, -16
139		call malloc@plt
140		cmp rax, 0
141		je _errOutOfMemory
142		mov rsp, rbp
143		pop rbp
144		ret
145	
146	.section .rodata
147	# length of .L._printi_str0
148		.int 2
149	.L._printi_str0:
150		.asciz "%d"
151	.text
152	_printi:
153		push rbp
154		mov rbp, rsp
155		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
156		and rsp, -16
157		mov esi, edi
158		lea rdi, [rip + .L._printi_str0]
159		# on x86, al represents the number of SIMD registers used as variadic arguments
160		mov al, 0
161		call printf@plt
162		mov rdi, 0
163		call fflush@plt
164		mov rsp, rbp
165		pop rbp
166		ret
167	
168	.section .rodata
169	# length of .L._printc_str0
170		.int 2
171	.L._printc_str0:
172		.asciz "%c"
173	.text
174	_printc:
175		push rbp
176		mov rbp, rsp
177		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
178		and rsp, -16
179		mov sil, dil
180		lea rdi, [rip + .L._printc_str0]
181		# on x86, al represents the number of SIMD registers used as variadic arguments
182		mov al, 0
183		call printf@plt
184		mov rdi, 0
185		call fflush@plt
186		mov rsp, rbp
187		pop rbp
188		ret
189	
190	.section .rodata
191	# length of .L._println_str0
192		.int 0
193	.L._println_str0:
194		.asciz ""
195	.text
196	_println:
197		push rbp
198		mov rbp, rsp
199		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
200		and rsp, -16
201		lea rdi, [rip + .L._println_str0]
202		call puts@plt
203		mov rdi, 0
204		call fflush@plt
205		mov rsp, rbp
206		pop rbp
207		ret
208	
209	_exit:
210		push rbp
211		mov rbp, rsp
212		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
213		and rsp, -16
214		call exit@plt
215		mov rsp, rbp
216		pop rbp
217		ret
218	
219	.section .rodata
220	# length of .L._errOutOfMemory_str0
221		.int 27
222	.L._errOutOfMemory_str0:
223		.asciz "fatal error: out of memory\n"
224	.text
225	_errOutOfMemory:
226		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
227		and rsp, -16
228		lea rdi, [rip + .L._errOutOfMemory_str0]
229		call _prints
230		mov dil, -1
231		call exit@plt
232	
233	.section .rodata
234	# length of .L._errNull_str0
235		.int 45
236	.L._errNull_str0:
237		.asciz "fatal error: null pair dereferenced or freed\n"
238	.text
239	_errNull:
240		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
241		and rsp, -16
242		lea rdi, [rip + .L._errNull_str0]
243		call _prints
244		mov dil, -1
245		call exit@plt
===========================================================
-- Finished

