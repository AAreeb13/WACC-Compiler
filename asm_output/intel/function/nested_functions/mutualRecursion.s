./valid/function/nested_functions/mutualRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/mutualRecursion.wacc
-- Test: mutualRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a pair of mutually recursive functions

# Output:
# r1: sending 8
# r2: received 8
# r1: sending 7
# r2: received 7
# r1: sending 6
# r2: received 6
# r1: sending 5
# r2: received 5
# r1: sending 4
# r2: received 4
# r1: sending 3
# r2: received 3
# r1: sending 2
# r2: received 2
# r1: sending 1
# r2: received 1
#

# Program:

begin
  int r1(int x) is
    if x == 0
    then
      skip
    else
      print "r1: sending " ;
      println x ;
      int y = call r2(x)
    fi ;
    return 42
  end

  int r2(int y) is
    print "r2: received " ;
    println y ;
    int z = call r1(y - 1) ;
    return 44
  end

  int x = 0 ;
  x = call r1(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
mutualRecursion.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 12
5	.L.str0:
6		.asciz "r1: sending "
7	# length of .L.str1
8		.int 13
9	.L.str1:
10		.asciz "r2: received "
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 0
21		mov r12, rax
22		# Stack pointer unchanged, no stack allocated arguments
23		mov rax, 8
24		mov rdi, rax
25		call wacc_r1
26		mov r11, rax
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r11
29		mov r12, rax
30		# Stack pointer unchanged, no stack allocated variables
31		mov rax, 0
32		# pop {rbx, r12}
33		mov rbx, qword ptr [rsp]
34		mov r12, qword ptr [rsp + 8]
35		add rsp, 16
36		pop rbp
37		ret
38	
39	wacc_r1:
40		push rbp
41		push r12
42		mov rbp, rsp
43		cmp rdi, 0
44		je .L0
45		# Stack pointer unchanged, no stack allocated variables
46		push rdi
47		# Set up R11 as a temporary second base pointer for the caller saved things
48		mov r11, rsp
49		# Stack pointer unchanged, no stack allocated arguments
50		lea rax, [rip + .L.str0]
51		push rax
52		pop rax
53		mov rax, rax
54		mov rdi, rax
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _prints
57		pop rdi
58		push rdi
59		# Set up R11 as a temporary second base pointer for the caller saved things
60		mov r11, rsp
61		# Stack pointer unchanged, no stack allocated arguments
62		mov rax, rdi
63		mov rdi, rax
64		# statement primitives do not return results (but will clobber r0/rax)
65		call _printi
66		call _println
67		pop rdi
68		push rdi
69		# Set up R11 as a temporary second base pointer for the caller saved things
70		mov r11, rsp
71		# Stack pointer unchanged, no stack allocated arguments
72		mov rax, rdi
73		mov rdi, rax
74		call wacc_r2
75		mov r11, rax
76		# Stack pointer unchanged, no stack allocated arguments
77		pop rdi
78		mov rax, r11
79		mov r12, rax
80		# Stack pointer unchanged, no stack allocated variables
81		jmp .L1
82	.L0:
83	.L1:
84		mov rax, 42
85		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
86		mov rsp, rbp
87		pop r12
88		pop rbp
89		ret
90		# 'ere be dragons: this is 100% dead code, functions always end in returns!
91	
92	wacc_r2:
93		push rbp
94		push r12
95		mov rbp, rsp
96		# Stack pointer unchanged, no stack allocated variables
97		push rdi
98		# Set up R11 as a temporary second base pointer for the caller saved things
99		mov r11, rsp
100		# Stack pointer unchanged, no stack allocated arguments
101		lea rax, [rip + .L.str1]
102		push rax
103		pop rax
104		mov rax, rax
105		mov rdi, rax
106		# statement primitives do not return results (but will clobber r0/rax)
107		call _prints
108		pop rdi
109		push rdi
110		# Set up R11 as a temporary second base pointer for the caller saved things
111		mov r11, rsp
112		# Stack pointer unchanged, no stack allocated arguments
113		mov rax, rdi
114		mov rdi, rax
115		# statement primitives do not return results (but will clobber r0/rax)
116		call _printi
117		call _println
118		pop rdi
119		push rdi
120		# Set up R11 as a temporary second base pointer for the caller saved things
121		mov r11, rsp
122		# Stack pointer unchanged, no stack allocated arguments
123		mov eax, edi
124		sub eax, 1
125		jo _errOverflow
126		movsx rax, eax
127		push rax
128		pop rax
129		mov rax, rax
130		mov rdi, rax
131		call wacc_r1
132		mov r11, rax
133		# Stack pointer unchanged, no stack allocated arguments
134		pop rdi
135		mov rax, r11
136		mov r12, rax
137		mov rax, 44
138		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
139		mov rsp, rbp
140		pop r12
141		pop rbp
142		ret
143		# Stack pointer unchanged, no stack allocated variables
144		# 'ere be dragons: this is 100% dead code, functions always end in returns!
145	
146	.section .rodata
147	# length of .L._prints_str0
148		.int 4
149	.L._prints_str0:
150		.asciz "%.*s"
151	.text
152	_prints:
153		push rbp
154		mov rbp, rsp
155		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
156		and rsp, -16
157		mov rdx, rdi
158		mov esi, dword ptr [rdi - 4]
159		lea rdi, [rip + .L._prints_str0]
160		# on x86, al represents the number of SIMD registers used as variadic arguments
161		mov al, 0
162		call printf@plt
163		mov rdi, 0
164		call fflush@plt
165		mov rsp, rbp
166		pop rbp
167		ret
168	
169	.section .rodata
170	# length of .L._printi_str0
171		.int 2
172	.L._printi_str0:
173		.asciz "%d"
174	.text
175	_printi:
176		push rbp
177		mov rbp, rsp
178		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
179		and rsp, -16
180		mov esi, edi
181		lea rdi, [rip + .L._printi_str0]
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
192	# length of .L._println_str0
193		.int 0
194	.L._println_str0:
195		.asciz ""
196	.text
197	_println:
198		push rbp
199		mov rbp, rsp
200		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
201		and rsp, -16
202		lea rdi, [rip + .L._println_str0]
203		call puts@plt
204		mov rdi, 0
205		call fflush@plt
206		mov rsp, rbp
207		pop rbp
208		ret
209	
210	_exit:
211		push rbp
212		mov rbp, rsp
213		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
214		and rsp, -16
215		call exit@plt
216		mov rsp, rbp
217		pop rbp
218		ret
219	
220	.section .rodata
221	# length of .L._errOverflow_str0
222		.int 52
223	.L._errOverflow_str0:
224		.asciz "fatal error: integer overflow or underflow occurred\n"
225	.text
226	_errOverflow:
227		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
228		and rsp, -16
229		lea rdi, [rip + .L._errOverflow_str0]
230		call _prints
231		mov dil, -1
232		call exit@plt
===========================================================
-- Finished

