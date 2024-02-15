./valid/function/nested_functions/printTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printTriangle.wacc
-- Test: printTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a fixed size triangle

# Output:
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

  int s = call f(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printTriangle.s contents are:
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
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, 8
22		mov rdi, rax
23		call wacc_f
24		mov r11, rax
25		# Stack pointer unchanged, no stack allocated arguments
26		mov rax, r11
27		mov r12, rax
28		# Stack pointer unchanged, no stack allocated variables
29		mov rax, 0
30		# pop {rbx, r12}
31		mov rbx, qword ptr [rsp]
32		mov r12, qword ptr [rsp + 8]
33		add rsp, 16
34		pop rbp
35		ret
36	
37	wacc_f:
38		push rbp
39		# push {r12, r13}
40		sub rsp, 16
41		mov qword ptr [rsp], r12
42		mov qword ptr [rsp + 8], r13
43		mov rbp, rsp
44		cmp rdi, 0
45		je .L0
46		# Stack pointer unchanged, no stack allocated variables
47		mov rax, rdi
48		mov r12, rax
49		jmp .L2
50	.L3:
51		push rdi
52		# Set up R11 as a temporary second base pointer for the caller saved things
53		mov r11, rsp
54		# Stack pointer unchanged, no stack allocated arguments
55		lea rax, [rip + .L.str0]
56		push rax
57		pop rax
58		mov rax, rax
59		mov rdi, rax
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _prints
62		pop rdi
63		mov eax, r12d
64		sub eax, 1
65		jo _errOverflow
66		movsx rax, eax
67		push rax
68		pop rax
69		mov rax, rax
70		mov r12, rax
71	.L2:
72		cmp r12, 0
73		jg .L3
74		push rdi
75		# Set up R11 as a temporary second base pointer for the caller saved things
76		mov r11, rsp
77		# Stack pointer unchanged, no stack allocated arguments
78		lea rax, [rip + .L.str1]
79		push rax
80		pop rax
81		mov rax, rax
82		mov rdi, rax
83		# statement primitives do not return results (but will clobber r0/rax)
84		call _prints
85		call _println
86		pop rdi
87		push rdi
88		# Set up R11 as a temporary second base pointer for the caller saved things
89		mov r11, rsp
90		# Stack pointer unchanged, no stack allocated arguments
91		mov eax, edi
92		sub eax, 1
93		jo _errOverflow
94		movsx rax, eax
95		push rax
96		pop rax
97		mov rax, rax
98		mov rdi, rax
99		call wacc_f
100		mov r11, rax
101		# Stack pointer unchanged, no stack allocated arguments
102		pop rdi
103		mov rax, r11
104		mov r13, rax
105		# Stack pointer unchanged, no stack allocated variables
106		jmp .L1
107	.L0:
108	.L1:
109		mov rax, 0
110		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
111		mov rsp, rbp
112		# pop {r12, r13}
113		mov r12, qword ptr [rsp]
114		mov r13, qword ptr [rsp + 8]
115		add rsp, 16
116		pop rbp
117		ret
118		# 'ere be dragons: this is 100% dead code, functions always end in returns!
119	
120	.section .rodata
121	# length of .L._prints_str0
122		.int 4
123	.L._prints_str0:
124		.asciz "%.*s"
125	.text
126	_prints:
127		push rbp
128		mov rbp, rsp
129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
130		and rsp, -16
131		mov rdx, rdi
132		mov esi, dword ptr [rdi - 4]
133		lea rdi, [rip + .L._prints_str0]
134		# on x86, al represents the number of SIMD registers used as variadic arguments
135		mov al, 0
136		call printf@plt
137		mov rdi, 0
138		call fflush@plt
139		mov rsp, rbp
140		pop rbp
141		ret
142	
143	.section .rodata
144	# length of .L._println_str0
145		.int 0
146	.L._println_str0:
147		.asciz ""
148	.text
149	_println:
150		push rbp
151		mov rbp, rsp
152		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
153		and rsp, -16
154		lea rdi, [rip + .L._println_str0]
155		call puts@plt
156		mov rdi, 0
157		call fflush@plt
158		mov rsp, rbp
159		pop rbp
160		ret
161	
162	_exit:
163		push rbp
164		mov rbp, rsp
165		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
166		and rsp, -16
167		call exit@plt
168		mov rsp, rbp
169		pop rbp
170		ret
171	
172	.section .rodata
173	# length of .L._errOverflow_str0
174		.int 52
175	.L._errOverflow_str0:
176		.asciz "fatal error: integer overflow or underflow occurred\n"
177	.text
178	_errOverflow:
179		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
180		and rsp, -16
181		lea rdi, [rip + .L._errOverflow_str0]
182		call _prints
183		mov dil, -1
184		call exit@plt
===========================================================
-- Finished

