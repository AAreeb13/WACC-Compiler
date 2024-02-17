./valid/scope/scopeIfRedefine.wacc
calling the reference compiler on ./valid/scope/scopeIfRedefine.wacc
-- Test: scopeIfRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within an if-statement

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  if x == 12 then
    bool x = true ;
    println x
  else
    char x = 'a';
    println x
  fi ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeIfRedefine.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13}
7		sub rsp, 24
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated variables
13		mov rax, 12
14		mov r12, rax
15		cmp r12, 12
16		je .L0
17		# Stack pointer unchanged, no stack allocated variables
18		mov rax, 97
19		mov r13, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		mov rax, r13
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printc
25		call _println
26		# Stack pointer unchanged, no stack allocated variables
27		jmp .L1
28	.L0:
29		# Stack pointer unchanged, no stack allocated variables
30		mov rax, 1
31		mov r13, rax
32		# Stack pointer unchanged, no stack allocated arguments
33		mov rax, r13
34		mov rdi, rax
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _printb
37		call _println
38		# Stack pointer unchanged, no stack allocated variables
39	.L1:
40		# Stack pointer unchanged, no stack allocated arguments
41		mov rax, r12
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printi
45		call _println
46		# Stack pointer unchanged, no stack allocated variables
47		mov rax, 0
48		# pop {rbx, r12, r13}
49		mov rbx, qword ptr [rsp]
50		mov r12, qword ptr [rsp + 8]
51		mov r13, qword ptr [rsp + 16]
52		add rsp, 24
53		pop rbp
54		ret
55	
56	.section .rodata
57	# length of .L._printi_str0
58		.int 2
59	.L._printi_str0:
60		.asciz "%d"
61	.text
62	_printi:
63		push rbp
64		mov rbp, rsp
65		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
66		and rsp, -16
67		mov esi, edi
68		lea rdi, [rip + .L._printi_str0]
69		# on x86, al represents the number of SIMD registers used as variadic arguments
70		mov al, 0
71		call printf@plt
72		mov rdi, 0
73		call fflush@plt
74		mov rsp, rbp
75		pop rbp
76		ret
77	
78	.section .rodata
79	# length of .L._printb_str0
80		.int 5
81	.L._printb_str0:
82		.asciz "false"
83	# length of .L._printb_str1
84		.int 4
85	.L._printb_str1:
86		.asciz "true"
87	# length of .L._printb_str2
88		.int 4
89	.L._printb_str2:
90		.asciz "%.*s"
91	.text
92	_printb:
93		push rbp
94		mov rbp, rsp
95		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
96		and rsp, -16
97		cmp dil, 0
98		jne .L_printb0
99		lea rdx, [rip + .L._printb_str0]
100		jmp .L_printb1
101	.L_printb0:
102		lea rdx, [rip + .L._printb_str1]
103	.L_printb1:
104		mov esi, dword ptr [rdx - 4]
105		lea rdi, [rip + .L._printb_str2]
106		# on x86, al represents the number of SIMD registers used as variadic arguments
107		mov al, 0
108		call printf@plt
109		mov rdi, 0
110		call fflush@plt
111		mov rsp, rbp
112		pop rbp
113		ret
114	
115	.section .rodata
116	# length of .L._printc_str0
117		.int 2
118	.L._printc_str0:
119		.asciz "%c"
120	.text
121	_printc:
122		push rbp
123		mov rbp, rsp
124		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
125		and rsp, -16
126		mov sil, dil
127		lea rdi, [rip + .L._printc_str0]
128		# on x86, al represents the number of SIMD registers used as variadic arguments
129		mov al, 0
130		call printf@plt
131		mov rdi, 0
132		call fflush@plt
133		mov rsp, rbp
134		pop rbp
135		ret
136	
137	.section .rodata
138	# length of .L._println_str0
139		.int 0
140	.L._println_str0:
141		.asciz ""
142	.text
143	_println:
144		push rbp
145		mov rbp, rsp
146		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
147		and rsp, -16
148		lea rdi, [rip + .L._println_str0]
149		call puts@plt
150		mov rdi, 0
151		call fflush@plt
152		mov rsp, rbp
153		pop rbp
154		ret
===========================================================
-- Finished

