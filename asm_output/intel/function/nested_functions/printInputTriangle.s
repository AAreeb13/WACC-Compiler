./valid/function/nested_functions/printInputTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printInputTriangle.wacc
-- Test: printInputTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a user-specified sized triangle

# Input: 13

# Output:
# Please enter the size of the triangle to print:
# -------------
# ------------
# -----------
# ----------
# ---------
# --------
# -------
# ------
# -----
# ----
# ---
# --
# -
#

# Program:

begin
  int f(int x) is
    if x == 0 then
      skip
    else
      int i = x ;
      while i > 0 do
        print "-" ;
        i = i - 1
      done ;
      println "" ;
      int s = call f(x - 1)
    fi ;
    return 0
  end

  println "Please enter the size of the triangle to print:" ;
  int x = 0;

  read x;
  int s = call f(x)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printInputTriangle.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 1
5	.L.str0:
6		.asciz "-"
7	# length of .L.str1
8		.int 0
9	.L.str1:
10		.asciz ""
11	# length of .L.str2
12		.int 47
13	.L.str2:
14		.asciz "Please enter the size of the triangle to print:"
15	.text
16	main:
17		push rbp
18		# push {rbx, r12, r13}
19		sub rsp, 24
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov rbp, rsp
24		# Stack pointer unchanged, no stack allocated variables
25		# Stack pointer unchanged, no stack allocated arguments
26		lea rax, [rip + .L.str2]
27		push rax
28		pop rax
29		mov rax, rax
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _prints
33		call _println
34		mov rax, 0
35		mov r12, rax
36		# Stack pointer unchanged, no stack allocated arguments
37		# load the current value in the destination of the read so it supports defaults
38		mov rax, r12
39		mov rdi, rax
40		call _readi
41		mov r11, rax
42		mov rax, r11
43		mov r12, rax
44		# Stack pointer unchanged, no stack allocated arguments
45		mov rax, r12
46		mov rdi, rax
47		call wacc_f
48		mov r11, rax
49		# Stack pointer unchanged, no stack allocated arguments
50		mov rax, r11
51		mov r13, rax
52		# Stack pointer unchanged, no stack allocated variables
53		mov rax, 0
54		# pop {rbx, r12, r13}
55		mov rbx, qword ptr [rsp]
56		mov r12, qword ptr [rsp + 8]
57		mov r13, qword ptr [rsp + 16]
58		add rsp, 24
59		pop rbp
60		ret
61	
62	wacc_f:
63		push rbp
64		# push {r12, r13}
65		sub rsp, 16
66		mov qword ptr [rsp], r12
67		mov qword ptr [rsp + 8], r13
68		mov rbp, rsp
69		cmp rdi, 0
70		je .L0
71		# Stack pointer unchanged, no stack allocated variables
72		mov rax, rdi
73		mov r12, rax
74		jmp .L2
75	.L3:
76		push rdi
77		# Set up R11 as a temporary second base pointer for the caller saved things
78		mov r11, rsp
79		# Stack pointer unchanged, no stack allocated arguments
80		lea rax, [rip + .L.str0]
81		push rax
82		pop rax
83		mov rax, rax
84		mov rdi, rax
85		# statement primitives do not return results (but will clobber r0/rax)
86		call _prints
87		pop rdi
88		mov eax, r12d
89		sub eax, 1
90		jo _errOverflow
91		movsx rax, eax
92		push rax
93		pop rax
94		mov rax, rax
95		mov r12, rax
96	.L2:
97		cmp r12, 0
98		jg .L3
99		push rdi
100		# Set up R11 as a temporary second base pointer for the caller saved things
101		mov r11, rsp
102		# Stack pointer unchanged, no stack allocated arguments
103		lea rax, [rip + .L.str1]
104		push rax
105		pop rax
106		mov rax, rax
107		mov rdi, rax
108		# statement primitives do not return results (but will clobber r0/rax)
109		call _prints
110		call _println
111		pop rdi
112		push rdi
113		# Set up R11 as a temporary second base pointer for the caller saved things
114		mov r11, rsp
115		# Stack pointer unchanged, no stack allocated arguments
116		mov eax, edi
117		sub eax, 1
118		jo _errOverflow
119		movsx rax, eax
120		push rax
121		pop rax
122		mov rax, rax
123		mov rdi, rax
124		call wacc_f
125		mov r11, rax
126		# Stack pointer unchanged, no stack allocated arguments
127		pop rdi
128		mov rax, r11
129		mov r13, rax
130		# Stack pointer unchanged, no stack allocated variables
131		jmp .L1
132	.L0:
133	.L1:
134		mov rax, 0
135		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
136		mov rsp, rbp
137		# pop {r12, r13}
138		mov r12, qword ptr [rsp]
139		mov r13, qword ptr [rsp + 8]
140		add rsp, 16
141		pop rbp
142		ret
143		# 'ere be dragons: this is 100% dead code, functions always end in returns!
144	
145	.section .rodata
146	# length of .L._prints_str0
147		.int 4
148	.L._prints_str0:
149		.asciz "%.*s"
150	.text
151	_prints:
152		push rbp
153		mov rbp, rsp
154		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
155		and rsp, -16
156		mov rdx, rdi
157		mov esi, dword ptr [rdi - 4]
158		lea rdi, [rip + .L._prints_str0]
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
169	# length of .L._readi_str0
170		.int 2
171	.L._readi_str0:
172		.asciz "%d"
173	.text
174	_readi:
175		push rbp
176		mov rbp, rsp
177		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
178		and rsp, -16
179		# RDI contains the "original" value of the destination of the read
180		# allocate space on the stack to store the read: preserve alignment!
181		# the passed default argument should be stored in case of EOF
182		sub rsp, 16
183		mov dword ptr [rsp], edi
184		lea rsi, qword ptr [rsp]
185		lea rdi, [rip + .L._readi_str0]
186		# on x86, al represents the number of SIMD registers used as variadic arguments
187		mov al, 0
188		call scanf@plt
189		movsx rax, dword ptr [rsp]
190		add rsp, 16
191		mov rsp, rbp
192		pop rbp
193		ret
194	
195	.section .rodata
196	# length of .L._println_str0
197		.int 0
198	.L._println_str0:
199		.asciz ""
200	.text
201	_println:
202		push rbp
203		mov rbp, rsp
204		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
205		and rsp, -16
206		lea rdi, [rip + .L._println_str0]
207		call puts@plt
208		mov rdi, 0
209		call fflush@plt
210		mov rsp, rbp
211		pop rbp
212		ret
213	
214	_exit:
215		push rbp
216		mov rbp, rsp
217		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
218		and rsp, -16
219		call exit@plt
220		mov rsp, rbp
221		pop rbp
222		ret
223	
224	.section .rodata
225	# length of .L._errOverflow_str0
226		.int 52
227	.L._errOverflow_str0:
228		.asciz "fatal error: integer overflow or underflow occurred\n"
229	.text
230	_errOverflow:
231		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
232		and rsp, -16
233		lea rdi, [rip + .L._errOverflow_str0]
234		call _prints
235		mov dil, -1
236		call exit@plt
===========================================================
-- Finished

