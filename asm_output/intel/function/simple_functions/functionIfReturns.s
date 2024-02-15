./valid/function/simple_functions/functionIfReturns.wacc
calling the reference compiler on ./valid/function/simple_functions/functionIfReturns.wacc
-- Test: functionIfReturns.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with nested returns inside an if-statement after a return

# Output:
# go
# 1
#

begin
    int f() is
        println "go";
        return 1;
        if true then
            println "a";
            return 2
        else
            println "b";
            return 3
        fi
    end

    int ret = call f();
    println ret
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionIfReturns.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 2
5	.L.str0:
6		.asciz "go"
7	# length of .L.str1
8		.int 1
9	.L.str1:
10		.asciz "b"
11	# length of .L.str2
12		.int 1
13	.L.str2:
14		.asciz "a"
15	.text
16	main:
17		push rbp
18		# push {rbx, r12}
19		sub rsp, 16
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov rbp, rsp
23		# Stack pointer unchanged, no stack allocated variables
24		# Stack pointer unchanged, no stack allocated arguments
25		call wacc_f
26		mov r11, rax
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r11
29		mov r12, rax
30		# Stack pointer unchanged, no stack allocated arguments
31		mov rax, r12
32		mov rdi, rax
33		# statement primitives do not return results (but will clobber r0/rax)
34		call _printi
35		call _println
36		# Stack pointer unchanged, no stack allocated variables
37		mov rax, 0
38		# pop {rbx, r12}
39		mov rbx, qword ptr [rsp]
40		mov r12, qword ptr [rsp + 8]
41		add rsp, 16
42		pop rbp
43		ret
44	
45	wacc_f:
46		push rbp
47		mov rbp, rsp
48		# Stack pointer unchanged, no stack allocated arguments
49		lea rax, [rip + .L.str0]
50		push rax
51		pop rax
52		mov rax, rax
53		mov rdi, rax
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _prints
56		call _println
57		mov rax, 1
58		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
59		mov rsp, rbp
60		pop rbp
61		ret
62		mov rax, 1
63		cmp rax, 1
64		je .L0
65		# Stack pointer unchanged, no stack allocated arguments
66		lea rax, [rip + .L.str1]
67		push rax
68		pop rax
69		mov rax, rax
70		mov rdi, rax
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _prints
73		call _println
74		mov rax, 3
75		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
76		mov rsp, rbp
77		pop rbp
78		ret
79		jmp .L1
80	.L0:
81		# Stack pointer unchanged, no stack allocated arguments
82		lea rax, [rip + .L.str2]
83		push rax
84		pop rax
85		mov rax, rax
86		mov rdi, rax
87		# statement primitives do not return results (but will clobber r0/rax)
88		call _prints
89		call _println
90		mov rax, 2
91		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
92		mov rsp, rbp
93		pop rbp
94		ret
95	.L1:
96		# 'ere be dragons: this is 100% dead code, functions always end in returns!
97	
98	.section .rodata
99	# length of .L._prints_str0
100		.int 4
101	.L._prints_str0:
102		.asciz "%.*s"
103	.text
104	_prints:
105		push rbp
106		mov rbp, rsp
107		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
108		and rsp, -16
109		mov rdx, rdi
110		mov esi, dword ptr [rdi - 4]
111		lea rdi, [rip + .L._prints_str0]
112		# on x86, al represents the number of SIMD registers used as variadic arguments
113		mov al, 0
114		call printf@plt
115		mov rdi, 0
116		call fflush@plt
117		mov rsp, rbp
118		pop rbp
119		ret
120	
121	.section .rodata
122	# length of .L._printi_str0
123		.int 2
124	.L._printi_str0:
125		.asciz "%d"
126	.text
127	_printi:
128		push rbp
129		mov rbp, rsp
130		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
131		and rsp, -16
132		mov esi, edi
133		lea rdi, [rip + .L._printi_str0]
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
===========================================================
-- Finished

