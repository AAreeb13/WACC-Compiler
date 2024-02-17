./valid/while/fibonacciFullIt.wacc
calling the reference compiler on ./valid/while/fibonacciFullIt.wacc
-- Test: fibonacciFullIt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iteratively calculate the given fibonacci number

# Input: 30

# Output:
# This program calculates the nth fibonacci number iteratively.
# Please enter n (should not be too large): The input n is 30
# The nth fibonacci number is 832040
#

# Program:

begin
  println "This program calculates the nth fibonacci number iteratively." ;
  print "Please enter n (should not be too large): " ;
  int n = 0;
  read n ;
  print "The input n is " ;
  println n ;
  print "The nth fibonacci number is " ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  while n > 0 do
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    n = n - 1
  done ;
  println f0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciFullIt.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 61
5	.L.str0:
6		.asciz "This program calculates the nth fibonacci number iteratively."
7	# length of .L.str1
8		.int 42
9	.L.str1:
10		.asciz "Please enter n (should not be too large): "
11	# length of .L.str2
12		.int 15
13	.L.str2:
14		.asciz "The input n is "
15	# length of .L.str3
16		.int 28
17	.L.str3:
18		.asciz "The nth fibonacci number is "
19	.text
20	main:
21		push rbp
22		# push {rbx, r12, r13, r14, r15}
23		sub rsp, 40
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov qword ptr [rsp + 16], r13
27		mov qword ptr [rsp + 24], r14
28		mov qword ptr [rsp + 32], r15
29		mov rbp, rsp
30		# Stack pointer unchanged, no stack allocated variables
31		# Stack pointer unchanged, no stack allocated arguments
32		lea rax, [rip + .L.str0]
33		push rax
34		pop rax
35		mov rax, rax
36		mov rdi, rax
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _prints
39		call _println
40		# Stack pointer unchanged, no stack allocated arguments
41		lea rax, [rip + .L.str1]
42		push rax
43		pop rax
44		mov rax, rax
45		mov rdi, rax
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _prints
48		mov rax, 0
49		mov r12, rax
50		# Stack pointer unchanged, no stack allocated arguments
51		# load the current value in the destination of the read so it supports defaults
52		mov rax, r12
53		mov rdi, rax
54		call _readi
55		mov r11, rax
56		mov rax, r11
57		mov r12, rax
58		# Stack pointer unchanged, no stack allocated arguments
59		lea rax, [rip + .L.str2]
60		push rax
61		pop rax
62		mov rax, rax
63		mov rdi, rax
64		# statement primitives do not return results (but will clobber r0/rax)
65		call _prints
66		# Stack pointer unchanged, no stack allocated arguments
67		mov rax, r12
68		mov rdi, rax
69		# statement primitives do not return results (but will clobber r0/rax)
70		call _printi
71		call _println
72		# Stack pointer unchanged, no stack allocated arguments
73		lea rax, [rip + .L.str3]
74		push rax
75		pop rax
76		mov rax, rax
77		mov rdi, rax
78		# statement primitives do not return results (but will clobber r0/rax)
79		call _prints
80		mov rax, 0
81		mov r13, rax
82		mov rax, 1
83		mov r14, rax
84		mov rax, 0
85		mov r15, rax
86		jmp .L0
87	.L1:
88		mov rax, r13
89		mov r15, rax
90		mov rax, r14
91		mov r13, rax
92		mov eax, r15d
93		add eax, r14d
94		jo _errOverflow
95		movsx rax, eax
96		push rax
97		pop rax
98		mov rax, rax
99		mov r14, rax
100		mov eax, r12d
101		sub eax, 1
102		jo _errOverflow
103		movsx rax, eax
104		push rax
105		pop rax
106		mov rax, rax
107		mov r12, rax
108	.L0:
109		cmp r12, 0
110		jg .L1
111		# Stack pointer unchanged, no stack allocated arguments
112		mov rax, r13
113		mov rdi, rax
114		# statement primitives do not return results (but will clobber r0/rax)
115		call _printi
116		call _println
117		# Stack pointer unchanged, no stack allocated variables
118		mov rax, 0
119		# pop {rbx, r12, r13, r14, r15}
120		mov rbx, qword ptr [rsp]
121		mov r12, qword ptr [rsp + 8]
122		mov r13, qword ptr [rsp + 16]
123		mov r14, qword ptr [rsp + 24]
124		mov r15, qword ptr [rsp + 32]
125		add rsp, 40
126		pop rbp
127		ret
128	
129	.section .rodata
130	# length of .L._prints_str0
131		.int 4
132	.L._prints_str0:
133		.asciz "%.*s"
134	.text
135	_prints:
136		push rbp
137		mov rbp, rsp
138		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
139		and rsp, -16
140		mov rdx, rdi
141		mov esi, dword ptr [rdi - 4]
142		lea rdi, [rip + .L._prints_str0]
143		# on x86, al represents the number of SIMD registers used as variadic arguments
144		mov al, 0
145		call printf@plt
146		mov rdi, 0
147		call fflush@plt
148		mov rsp, rbp
149		pop rbp
150		ret
151	
152	.section .rodata
153	# length of .L._readi_str0
154		.int 2
155	.L._readi_str0:
156		.asciz "%d"
157	.text
158	_readi:
159		push rbp
160		mov rbp, rsp
161		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
162		and rsp, -16
163		# RDI contains the "original" value of the destination of the read
164		# allocate space on the stack to store the read: preserve alignment!
165		# the passed default argument should be stored in case of EOF
166		sub rsp, 16
167		mov dword ptr [rsp], edi
168		lea rsi, qword ptr [rsp]
169		lea rdi, [rip + .L._readi_str0]
170		# on x86, al represents the number of SIMD registers used as variadic arguments
171		mov al, 0
172		call scanf@plt
173		movsx rax, dword ptr [rsp]
174		add rsp, 16
175		mov rsp, rbp
176		pop rbp
177		ret
178	
179	.section .rodata
180	# length of .L._printi_str0
181		.int 2
182	.L._printi_str0:
183		.asciz "%d"
184	.text
185	_printi:
186		push rbp
187		mov rbp, rsp
188		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
189		and rsp, -16
190		mov esi, edi
191		lea rdi, [rip + .L._printi_str0]
192		# on x86, al represents the number of SIMD registers used as variadic arguments
193		mov al, 0
194		call printf@plt
195		mov rdi, 0
196		call fflush@plt
197		mov rsp, rbp
198		pop rbp
199		ret
200	
201	.section .rodata
202	# length of .L._println_str0
203		.int 0
204	.L._println_str0:
205		.asciz ""
206	.text
207	_println:
208		push rbp
209		mov rbp, rsp
210		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
211		and rsp, -16
212		lea rdi, [rip + .L._println_str0]
213		call puts@plt
214		mov rdi, 0
215		call fflush@plt
216		mov rsp, rbp
217		pop rbp
218		ret
219	
220	_exit:
221		push rbp
222		mov rbp, rsp
223		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
224		and rsp, -16
225		call exit@plt
226		mov rsp, rbp
227		pop rbp
228		ret
229	
230	.section .rodata
231	# length of .L._errOverflow_str0
232		.int 52
233	.L._errOverflow_str0:
234		.asciz "fatal error: integer overflow or underflow occurred\n"
235	.text
236	_errOverflow:
237		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
238		and rsp, -16
239		lea rdi, [rip + .L._errOverflow_str0]
240		call _prints
241		mov dil, -1
242		call exit@plt
===========================================================
-- Finished

