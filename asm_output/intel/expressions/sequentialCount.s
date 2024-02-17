./valid/expressions/sequentialCount.wacc
calling the reference compiler on ./valid/expressions/sequentialCount.wacc
-- Test: sequentialCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple sequential counting

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i ;
  i = i + 1 ;
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
sequentialCount.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 20
5	.L.str0:
6		.asciz "Can you count to 10?"
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		mov rax, 1
17		mov r12, rax
18		# Stack pointer unchanged, no stack allocated arguments
19		lea rax, [rip + .L.str0]
20		push rax
21		pop rax
22		mov rax, rax
23		mov rdi, rax
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _prints
26		call _println
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r12
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printi
32		call _println
33		mov eax, r12d
34		add eax, 1
35		jo _errOverflow
36		movsx rax, eax
37		push rax
38		pop rax
39		mov rax, rax
40		mov r12, rax
41		# Stack pointer unchanged, no stack allocated arguments
42		mov rax, r12
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _printi
46		call _println
47		mov eax, r12d
48		add eax, 1
49		jo _errOverflow
50		movsx rax, eax
51		push rax
52		pop rax
53		mov rax, rax
54		mov r12, rax
55		# Stack pointer unchanged, no stack allocated arguments
56		mov rax, r12
57		mov rdi, rax
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _printi
60		call _println
61		mov eax, r12d
62		add eax, 1
63		jo _errOverflow
64		movsx rax, eax
65		push rax
66		pop rax
67		mov rax, rax
68		mov r12, rax
69		# Stack pointer unchanged, no stack allocated arguments
70		mov rax, r12
71		mov rdi, rax
72		# statement primitives do not return results (but will clobber r0/rax)
73		call _printi
74		call _println
75		mov eax, r12d
76		add eax, 1
77		jo _errOverflow
78		movsx rax, eax
79		push rax
80		pop rax
81		mov rax, rax
82		mov r12, rax
83		# Stack pointer unchanged, no stack allocated arguments
84		mov rax, r12
85		mov rdi, rax
86		# statement primitives do not return results (but will clobber r0/rax)
87		call _printi
88		call _println
89		mov eax, r12d
90		add eax, 1
91		jo _errOverflow
92		movsx rax, eax
93		push rax
94		pop rax
95		mov rax, rax
96		mov r12, rax
97		# Stack pointer unchanged, no stack allocated arguments
98		mov rax, r12
99		mov rdi, rax
100		# statement primitives do not return results (but will clobber r0/rax)
101		call _printi
102		call _println
103		mov eax, r12d
104		add eax, 1
105		jo _errOverflow
106		movsx rax, eax
107		push rax
108		pop rax
109		mov rax, rax
110		mov r12, rax
111		# Stack pointer unchanged, no stack allocated arguments
112		mov rax, r12
113		mov rdi, rax
114		# statement primitives do not return results (but will clobber r0/rax)
115		call _printi
116		call _println
117		mov eax, r12d
118		add eax, 1
119		jo _errOverflow
120		movsx rax, eax
121		push rax
122		pop rax
123		mov rax, rax
124		mov r12, rax
125		# Stack pointer unchanged, no stack allocated arguments
126		mov rax, r12
127		mov rdi, rax
128		# statement primitives do not return results (but will clobber r0/rax)
129		call _printi
130		call _println
131		mov eax, r12d
132		add eax, 1
133		jo _errOverflow
134		movsx rax, eax
135		push rax
136		pop rax
137		mov rax, rax
138		mov r12, rax
139		# Stack pointer unchanged, no stack allocated arguments
140		mov rax, r12
141		mov rdi, rax
142		# statement primitives do not return results (but will clobber r0/rax)
143		call _printi
144		call _println
145		mov eax, r12d
146		add eax, 1
147		jo _errOverflow
148		movsx rax, eax
149		push rax
150		pop rax
151		mov rax, rax
152		mov r12, rax
153		# Stack pointer unchanged, no stack allocated arguments
154		mov rax, r12
155		mov rdi, rax
156		# statement primitives do not return results (but will clobber r0/rax)
157		call _printi
158		call _println
159		# Stack pointer unchanged, no stack allocated variables
160		mov rax, 0
161		# pop {rbx, r12}
162		mov rbx, qword ptr [rsp]
163		mov r12, qword ptr [rsp + 8]
164		add rsp, 16
165		pop rbp
166		ret
167	
168	.section .rodata
169	# length of .L._prints_str0
170		.int 4
171	.L._prints_str0:
172		.asciz "%.*s"
173	.text
174	_prints:
175		push rbp
176		mov rbp, rsp
177		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
178		and rsp, -16
179		mov rdx, rdi
180		mov esi, dword ptr [rdi - 4]
181		lea rdi, [rip + .L._prints_str0]
182		# on x86, al represents the number of SIMD registers used as variadic arguments
183		mov al, 0
184		call printf@plt
185		mov rdi, 0
186		call fflush@plt
187		mov rsp, rbp
188		pop rbp
189		ret
190	
191	.section .rodata
192	# length of .L._printi_str0
193		.int 2
194	.L._printi_str0:
195		.asciz "%d"
196	.text
197	_printi:
198		push rbp
199		mov rbp, rsp
200		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
201		and rsp, -16
202		mov esi, edi
203		lea rdi, [rip + .L._printi_str0]
204		# on x86, al represents the number of SIMD registers used as variadic arguments
205		mov al, 0
206		call printf@plt
207		mov rdi, 0
208		call fflush@plt
209		mov rsp, rbp
210		pop rbp
211		ret
212	
213	.section .rodata
214	# length of .L._println_str0
215		.int 0
216	.L._println_str0:
217		.asciz ""
218	.text
219	_println:
220		push rbp
221		mov rbp, rsp
222		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
223		and rsp, -16
224		lea rdi, [rip + .L._println_str0]
225		call puts@plt
226		mov rdi, 0
227		call fflush@plt
228		mov rsp, rbp
229		pop rbp
230		ret
231	
232	_exit:
233		push rbp
234		mov rbp, rsp
235		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
236		and rsp, -16
237		call exit@plt
238		mov rsp, rbp
239		pop rbp
240		ret
241	
242	.section .rodata
243	# length of .L._errOverflow_str0
244		.int 52
245	.L._errOverflow_str0:
246		.asciz "fatal error: integer overflow or underflow occurred\n"
247	.text
248	_errOverflow:
249		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
250		and rsp, -16
251		lea rdi, [rip + .L._errOverflow_str0]
252		call _prints
253		mov dil, -1
254		call exit@plt
===========================================================
-- Finished

