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
0	.data
1	@ length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz "go"
5	@ length of .L.str1
6		.word 1
7	.L.str1:
8		.asciz "b"
9	@ length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz "a"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		@ Stack pointer unchanged, no stack allocated arguments
22		bl wacc_f
23		mov r12, r0
24		@ Stack pointer unchanged, no stack allocated arguments
25		mov r8, r12
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated arguments
28		mov r8, r4
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _printi
32		bl _println
33		@ Stack pointer unchanged, no stack allocated variables
34		mov r0, #0
35		pop {r4, r8, r10, r12}
36		pop {fp, pc}
37	
38	wacc_f:
39		push {fp, lr}
40		mov fp, sp
41		@ Stack pointer unchanged, no stack allocated arguments
42		ldr r8, =.L.str0
43		push {r8}
44		pop {r8}
45		mov r8, r8
46		mov r0, r8
47		@ statement primitives do not return results (but will clobber r0/rax)
48		bl _prints
49		bl _println
50		mov r0, #1
51		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
52		mov sp, fp
53		pop {fp, pc}
54		mov r8, #1
55		cmp r8, #1
56		beq .L0
57		@ Stack pointer unchanged, no stack allocated arguments
58		ldr r8, =.L.str1
59		push {r8}
60		pop {r8}
61		mov r8, r8
62		mov r0, r8
63		@ statement primitives do not return results (but will clobber r0/rax)
64		bl _prints
65		bl _println
66		mov r0, #3
67		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
68		mov sp, fp
69		pop {fp, pc}
70		b .L1
71	.L0:
72		@ Stack pointer unchanged, no stack allocated arguments
73		ldr r8, =.L.str2
74		push {r8}
75		pop {r8}
76		mov r8, r8
77		mov r0, r8
78		@ statement primitives do not return results (but will clobber r0/rax)
79		bl _prints
80		bl _println
81		mov r0, #2
82		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
83		mov sp, fp
84		pop {fp, pc}
85	.L1:
86		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
87		@ the .ltorg here is used to generate the constant pool after every function
88		@ this is important for ensuring string literals and large integers are accessible!
89		.ltorg
90	
91	@ length of .L._prints_str0
92		.word 4
93	.L._prints_str0:
94		.asciz "%.*s"
95	.align 4
96	_prints:
97		push {fp, lr}
98		mov fp, sp
99		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
100		bic sp, sp, #0x7
101		mov r2, r0
102		ldr r1, [r0, #-4]
103		adr r0, .L._prints_str0
104		bl printf
105		mov r0, #0
106		bl fflush
107		mov sp, fp
108		pop {fp, pc}
109	
110	@ length of .L._printi_str0
111		.word 2
112	.L._printi_str0:
113		.asciz "%d"
114	.align 4
115	_printi:
116		push {fp, lr}
117		mov fp, sp
118		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
119		bic sp, sp, #0x7
120		mov r1, r0
121		adr r0, .L._printi_str0
122		bl printf
123		mov r0, #0
124		bl fflush
125		mov sp, fp
126		pop {fp, pc}
127	
128	@ length of .L._println_str0
129		.word 0
130	.L._println_str0:
131		.asciz ""
132	.align 4
133	_println:
134		push {fp, lr}
135		mov fp, sp
136		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
137		bic sp, sp, #0x7
138		adr r0, .L._println_str0
139		bl puts
140		mov r0, #0
141		bl fflush
142		mov sp, fp
143		pop {fp, pc}
===========================================================
-- Finished

