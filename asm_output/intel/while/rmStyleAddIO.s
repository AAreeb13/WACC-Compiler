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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 24
5	.L.str0:
6		.asciz "Enter the first number: "
7	# length of .L.str1
8		.int 25
9	.L.str1:
10		.asciz "Enter the second number: "
11	# length of .L.str2
12		.int 20
13	.L.str2:
14		.asciz "Initial value of x: "
15	# length of .L.str3
16		.int 3
17	.L.str3:
18		.asciz "(+)"
19	# length of .L.str4
20		.int 0
21	.L.str4:
22		.asciz ""
23	# length of .L.str5
24		.int 18
25	.L.str5:
26		.asciz "final value of x: "
27	.text
28	main:
29		push rbp
30		# push {rbx, r12, r13}
31		sub rsp, 24
32		mov qword ptr [rsp], rbx
33		mov qword ptr [rsp + 8], r12
34		mov qword ptr [rsp + 16], r13
35		mov rbp, rsp
36		# Stack pointer unchanged, no stack allocated variables
37		mov rax, 0
38		mov r12, rax
39		mov rax, 0
40		mov r13, rax
41		# Stack pointer unchanged, no stack allocated arguments
42		lea rax, [rip + .L.str0]
43		push rax
44		pop rax
45		mov rax, rax
46		mov rdi, rax
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _prints
49		# Stack pointer unchanged, no stack allocated arguments
50		# load the current value in the destination of the read so it supports defaults
51		mov rax, r12
52		mov rdi, rax
53		call _readi
54		mov r11, rax
55		mov rax, r11
56		mov r12, rax
57		# Stack pointer unchanged, no stack allocated arguments
58		lea rax, [rip + .L.str1]
59		push rax
60		pop rax
61		mov rax, rax
62		mov rdi, rax
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _prints
65		# Stack pointer unchanged, no stack allocated arguments
66		# load the current value in the destination of the read so it supports defaults
67		mov rax, r13
68		mov rdi, rax
69		call _readi
70		mov r11, rax
71		mov rax, r11
72		mov r13, rax
73		# Stack pointer unchanged, no stack allocated arguments
74		lea rax, [rip + .L.str2]
75		push rax
76		pop rax
77		mov rax, rax
78		mov rdi, rax
79		# statement primitives do not return results (but will clobber r0/rax)
80		call _prints
81		# Stack pointer unchanged, no stack allocated arguments
82		mov rax, r12
83		mov rdi, rax
84		# statement primitives do not return results (but will clobber r0/rax)
85		call _printi
86		call _println
87		jmp .L0
88	.L1:
89		# Stack pointer unchanged, no stack allocated arguments
90		lea rax, [rip + .L.str3]
91		push rax
92		pop rax
93		mov rax, rax
94		mov rdi, rax
95		# statement primitives do not return results (but will clobber r0/rax)
96		call _prints
97		mov eax, r12d
98		add eax, 1
99		jo _errOverflow
100		movsx rax, eax
101		push rax
102		pop rax
103		mov rax, rax
104		mov r12, rax
105		mov eax, r13d
106		sub eax, 1
107		jo _errOverflow
108		movsx rax, eax
109		push rax
110		pop rax
111		mov rax, rax
112		mov r13, rax
113	.L0:
114		cmp r13, 0
115		jg .L1
116		# Stack pointer unchanged, no stack allocated arguments
117		lea rax, [rip + .L.str4]
118		push rax
119		pop rax
120		mov rax, rax
121		mov rdi, rax
122		# statement primitives do not return results (but will clobber r0/rax)
123		call _prints
124		call _println
125		# Stack pointer unchanged, no stack allocated arguments
126		lea rax, [rip + .L.str5]
127		push rax
128		pop rax
129		mov rax, rax
130		mov rdi, rax
131		# statement primitives do not return results (but will clobber r0/rax)
132		call _prints
133		# Stack pointer unchanged, no stack allocated arguments
134		mov rax, r12
135		mov rdi, rax
136		# statement primitives do not return results (but will clobber r0/rax)
137		call _printi
138		call _println
139		# Stack pointer unchanged, no stack allocated variables
140		mov rax, 0
141		# pop {rbx, r12, r13}
142		mov rbx, qword ptr [rsp]
143		mov r12, qword ptr [rsp + 8]
144		mov r13, qword ptr [rsp + 16]
145		add rsp, 24
146		pop rbp
147		ret
148	
149	.section .rodata
150	# length of .L._prints_str0
151		.int 4
152	.L._prints_str0:
153		.asciz "%.*s"
154	.text
155	_prints:
156		push rbp
157		mov rbp, rsp
158		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
159		and rsp, -16
160		mov rdx, rdi
161		mov esi, dword ptr [rdi - 4]
162		lea rdi, [rip + .L._prints_str0]
163		# on x86, al represents the number of SIMD registers used as variadic arguments
164		mov al, 0
165		call printf@plt
166		mov rdi, 0
167		call fflush@plt
168		mov rsp, rbp
169		pop rbp
170		ret
171	
172	.section .rodata
173	# length of .L._readi_str0
174		.int 2
175	.L._readi_str0:
176		.asciz "%d"
177	.text
178	_readi:
179		push rbp
180		mov rbp, rsp
181		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
182		and rsp, -16
183		# RDI contains the "original" value of the destination of the read
184		# allocate space on the stack to store the read: preserve alignment!
185		# the passed default argument should be stored in case of EOF
186		sub rsp, 16
187		mov dword ptr [rsp], edi
188		lea rsi, qword ptr [rsp]
189		lea rdi, [rip + .L._readi_str0]
190		# on x86, al represents the number of SIMD registers used as variadic arguments
191		mov al, 0
192		call scanf@plt
193		movsx rax, dword ptr [rsp]
194		add rsp, 16
195		mov rsp, rbp
196		pop rbp
197		ret
198	
199	.section .rodata
200	# length of .L._printi_str0
201		.int 2
202	.L._printi_str0:
203		.asciz "%d"
204	.text
205	_printi:
206		push rbp
207		mov rbp, rsp
208		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
209		and rsp, -16
210		mov esi, edi
211		lea rdi, [rip + .L._printi_str0]
212		# on x86, al represents the number of SIMD registers used as variadic arguments
213		mov al, 0
214		call printf@plt
215		mov rdi, 0
216		call fflush@plt
217		mov rsp, rbp
218		pop rbp
219		ret
220	
221	.section .rodata
222	# length of .L._println_str0
223		.int 0
224	.L._println_str0:
225		.asciz ""
226	.text
227	_println:
228		push rbp
229		mov rbp, rsp
230		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
231		and rsp, -16
232		lea rdi, [rip + .L._println_str0]
233		call puts@plt
234		mov rdi, 0
235		call fflush@plt
236		mov rsp, rbp
237		pop rbp
238		ret
239	
240	_exit:
241		push rbp
242		mov rbp, rsp
243		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
244		and rsp, -16
245		call exit@plt
246		mov rsp, rbp
247		pop rbp
248		ret
249	
250	.section .rodata
251	# length of .L._errOverflow_str0
252		.int 52
253	.L._errOverflow_str0:
254		.asciz "fatal error: integer overflow or underflow occurred\n"
255	.text
256	_errOverflow:
257		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
258		and rsp, -16
259		lea rdi, [rip + .L._errOverflow_str0]
260		call _prints
261		mov dil, -1
262		call exit@plt
===========================================================
-- Finished

