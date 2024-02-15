./valid/function/simple_functions/lotsOfLocals.wacc
calling the reference compiler on ./valid/function/simple_functions/lotsOfLocals.wacc
-- Test: lotsOfLocals.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Functions with lots of local arguments should work ok
# Thanks to Kristina Zimina for finding this

# Output:
# 5
# 8
#

# Program:

begin
    int f (int a, int b, int c, int d, int e, int k) is
        int x1 = 1;
        int x2 = 2;
        int x3 = 3;
        int x4 = 4;
        char x5 = '5';
        char x6 = '6';
        char x7 = '7';
        int x8 = 8;
        begin
            char y1 = 'a';
            char y2 = 'b';
            int y = 5 ;
            println y
        end ;
        return x8
    end

    int r = call f(1, 2, 3, 4, 5, 6);
    println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lotsOfLocals.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12}
7		sub rsp, 16
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov rbp, rsp
11		# Stack pointer unchanged, no stack allocated variables
12		# Stack pointer unchanged, no stack allocated arguments
13		mov rax, 1
14		mov rdi, rax
15		mov rax, 2
16		mov rsi, rax
17		mov rax, 3
18		mov rdx, rax
19		mov rax, 4
20		mov rcx, rax
21		mov rax, 5
22		mov r8, rax
23		mov rax, 6
24		mov r9, rax
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
47		# push {r12, r13, r14, r15}
48		sub rsp, 32
49		mov qword ptr [rsp], r12
50		mov qword ptr [rsp + 8], r13
51		mov qword ptr [rsp + 16], r14
52		mov qword ptr [rsp + 24], r15
53		mov rbp, rsp
54		sub rsp, 7
55		mov rax, 1
56		mov r12, rax
57		mov rax, 2
58		mov r13, rax
59		mov rax, 3
60		mov r14, rax
61		mov rax, 4
62		mov r15, rax
63		mov rax, 53
64		mov byte ptr [rbp - 7], al
65		mov rax, 54
66		mov byte ptr [rbp - 6], al
67		mov rax, 55
68		mov byte ptr [rbp - 5], al
69		mov rax, 8
70		mov dword ptr [rbp - 4], eax
71		sub rsp, 6
72		mov rax, 97
73		mov byte ptr [rbp - 13], al
74		mov rax, 98
75		mov byte ptr [rbp - 12], al
76		mov rax, 5
77		mov dword ptr [rbp - 11], eax
78		# push {rcx, rdx, rsi, rdi, r8, r9}
79		sub rsp, 48
80		mov qword ptr [rsp], rcx
81		mov qword ptr [rsp + 8], rdx
82		mov qword ptr [rsp + 16], rsi
83		mov qword ptr [rsp + 24], rdi
84		mov qword ptr [rsp + 32], r8
85		mov qword ptr [rsp + 40], r9
86		# Set up R11 as a temporary second base pointer for the caller saved things
87		mov r11, rsp
88		# Stack pointer unchanged, no stack allocated arguments
89		movsx rax, dword ptr [rbp - 11]
90		mov rax, rax
91		mov rdi, rax
92		# statement primitives do not return results (but will clobber r0/rax)
93		call _printi
94		call _println
95		# pop {rcx, rdx, rsi, rdi, r8, r9}
96		mov rcx, qword ptr [rsp]
97		mov rdx, qword ptr [rsp + 8]
98		mov rsi, qword ptr [rsp + 16]
99		mov rdi, qword ptr [rsp + 24]
100		mov r8, qword ptr [rsp + 32]
101		mov r9, qword ptr [rsp + 40]
102		add rsp, 48
103		add rsp, 6
104		movsx rax, dword ptr [rbp - 4]
105		mov rax, rax
106		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
107		mov rsp, rbp
108		# pop {r12, r13, r14, r15}
109		mov r12, qword ptr [rsp]
110		mov r13, qword ptr [rsp + 8]
111		mov r14, qword ptr [rsp + 16]
112		mov r15, qword ptr [rsp + 24]
113		add rsp, 32
114		pop rbp
115		ret
116		add rsp, 7
117		# 'ere be dragons: this is 100% dead code, functions always end in returns!
118	
119	.section .rodata
120	# length of .L._printi_str0
121		.int 2
122	.L._printi_str0:
123		.asciz "%d"
124	.text
125	_printi:
126		push rbp
127		mov rbp, rsp
128		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
129		and rsp, -16
130		mov esi, edi
131		lea rdi, [rip + .L._printi_str0]
132		# on x86, al represents the number of SIMD registers used as variadic arguments
133		mov al, 0
134		call printf@plt
135		mov rdi, 0
136		call fflush@plt
137		mov rsp, rbp
138		pop rbp
139		ret
140	
141	.section .rodata
142	# length of .L._println_str0
143		.int 0
144	.L._println_str0:
145		.asciz ""
146	.text
147	_println:
148		push rbp
149		mov rbp, rsp
150		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
151		and rsp, -16
152		lea rdi, [rip + .L._println_str0]
153		call puts@plt
154		mov rdi, 0
155		call fflush@plt
156		mov rsp, rbp
157		pop rbp
158		ret
===========================================================
-- Finished

