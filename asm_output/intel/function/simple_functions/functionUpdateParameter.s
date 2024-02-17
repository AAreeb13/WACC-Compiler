./valid/function/simple_functions/functionUpdateParameter.wacc
calling the reference compiler on ./valid/function/simple_functions/functionUpdateParameter.wacc
-- Test: functionUpdateParameter.wacc

-- Uploaded file: 
---------------------------------------------------------------
# test that the passed parameter can be updated and used
# and that y remains the same

# Output:
# y is 1
# x is 1
# x is now 5
# y is still 1
#

# Program:

begin

  int f(int x) is
    print "x is ";
    println x;
    x = 5;
    print "x is now ";
    println x;
    return x
  end

  int y = 1;
  print "y is ";
  println y;
  int x = call f(y);
  print "y is still ";
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionUpdateParameter.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 5
5	.L.str0:
6		.asciz "x is "
7	# length of .L.str1
8		.int 9
9	.L.str1:
10		.asciz "x is now "
11	# length of .L.str2
12		.int 5
13	.L.str2:
14		.asciz "y is "
15	# length of .L.str3
16		.int 11
17	.L.str3:
18		.asciz "y is still "
19	.text
20	main:
21		push rbp
22		# push {rbx, r12, r13}
23		sub rsp, 24
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov qword ptr [rsp + 16], r13
27		mov rbp, rsp
28		# Stack pointer unchanged, no stack allocated variables
29		mov rax, 1
30		mov r12, rax
31		# Stack pointer unchanged, no stack allocated arguments
32		lea rax, [rip + .L.str2]
33		push rax
34		pop rax
35		mov rax, rax
36		mov rdi, rax
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _prints
39		# Stack pointer unchanged, no stack allocated arguments
40		mov rax, r12
41		mov rdi, rax
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printi
44		call _println
45		# Stack pointer unchanged, no stack allocated arguments
46		mov rax, r12
47		mov rdi, rax
48		call wacc_f
49		mov r11, rax
50		# Stack pointer unchanged, no stack allocated arguments
51		mov rax, r11
52		mov r13, rax
53		# Stack pointer unchanged, no stack allocated arguments
54		lea rax, [rip + .L.str3]
55		push rax
56		pop rax
57		mov rax, rax
58		mov rdi, rax
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _prints
61		# Stack pointer unchanged, no stack allocated arguments
62		mov rax, r12
63		mov rdi, rax
64		# statement primitives do not return results (but will clobber r0/rax)
65		call _printi
66		call _println
67		# Stack pointer unchanged, no stack allocated variables
68		mov rax, 0
69		# pop {rbx, r12, r13}
70		mov rbx, qword ptr [rsp]
71		mov r12, qword ptr [rsp + 8]
72		mov r13, qword ptr [rsp + 16]
73		add rsp, 24
74		pop rbp
75		ret
76	
77	wacc_f:
78		push rbp
79		mov rbp, rsp
80		push rdi
81		# Set up R11 as a temporary second base pointer for the caller saved things
82		mov r11, rsp
83		# Stack pointer unchanged, no stack allocated arguments
84		lea rax, [rip + .L.str0]
85		push rax
86		pop rax
87		mov rax, rax
88		mov rdi, rax
89		# statement primitives do not return results (but will clobber r0/rax)
90		call _prints
91		pop rdi
92		push rdi
93		# Set up R11 as a temporary second base pointer for the caller saved things
94		mov r11, rsp
95		# Stack pointer unchanged, no stack allocated arguments
96		mov rax, rdi
97		mov rdi, rax
98		# statement primitives do not return results (but will clobber r0/rax)
99		call _printi
100		call _println
101		pop rdi
102		mov rax, 5
103		mov rdi, rax
104		push rdi
105		# Set up R11 as a temporary second base pointer for the caller saved things
106		mov r11, rsp
107		# Stack pointer unchanged, no stack allocated arguments
108		lea rax, [rip + .L.str1]
109		push rax
110		pop rax
111		mov rax, rax
112		mov rdi, rax
113		# statement primitives do not return results (but will clobber r0/rax)
114		call _prints
115		pop rdi
116		push rdi
117		# Set up R11 as a temporary second base pointer for the caller saved things
118		mov r11, rsp
119		# Stack pointer unchanged, no stack allocated arguments
120		mov rax, rdi
121		mov rdi, rax
122		# statement primitives do not return results (but will clobber r0/rax)
123		call _printi
124		call _println
125		pop rdi
126		mov rax, rdi
127		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
128		mov rsp, rbp
129		pop rbp
130		ret
131		# 'ere be dragons: this is 100% dead code, functions always end in returns!
132	
133	.section .rodata
134	# length of .L._prints_str0
135		.int 4
136	.L._prints_str0:
137		.asciz "%.*s"
138	.text
139	_prints:
140		push rbp
141		mov rbp, rsp
142		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
143		and rsp, -16
144		mov rdx, rdi
145		mov esi, dword ptr [rdi - 4]
146		lea rdi, [rip + .L._prints_str0]
147		# on x86, al represents the number of SIMD registers used as variadic arguments
148		mov al, 0
149		call printf@plt
150		mov rdi, 0
151		call fflush@plt
152		mov rsp, rbp
153		pop rbp
154		ret
155	
156	.section .rodata
157	# length of .L._printi_str0
158		.int 2
159	.L._printi_str0:
160		.asciz "%d"
161	.text
162	_printi:
163		push rbp
164		mov rbp, rsp
165		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
166		and rsp, -16
167		mov esi, edi
168		lea rdi, [rip + .L._printi_str0]
169		# on x86, al represents the number of SIMD registers used as variadic arguments
170		mov al, 0
171		call printf@plt
172		mov rdi, 0
173		call fflush@plt
174		mov rsp, rbp
175		pop rbp
176		ret
177	
178	.section .rodata
179	# length of .L._println_str0
180		.int 0
181	.L._println_str0:
182		.asciz ""
183	.text
184	_println:
185		push rbp
186		mov rbp, rsp
187		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
188		and rsp, -16
189		lea rdi, [rip + .L._println_str0]
190		call puts@plt
191		mov rdi, 0
192		call fflush@plt
193		mov rsp, rbp
194		pop rbp
195		ret
===========================================================
-- Finished

