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
1	// length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz "go"
5	// length of .L.str1
6		.word 1
7	.L.str1:
8		.asciz "b"
9	// length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz "a"
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19}
20		stp x19, xzr, [sp, #-16]!
21		mov fp, sp
22		// Stack pointer unchanged, no stack allocated variables
23		// Stack pointer unchanged, no stack allocated arguments
24		bl wacc_f
25		mov x16, x0
26		// Stack pointer unchanged, no stack allocated arguments
27		mov x8, x16
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated arguments
30		mov x8, x19
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _printi
34		bl _println
35		// Stack pointer unchanged, no stack allocated variables
36		mov x0, #0
37		// pop {x19}
38		ldp x19, xzr, [sp], #16
39		// pop {fp, lr}
40		ldp fp, lr, [sp], #16
41		ret
42	
43	wacc_f:
44		// push {fp, lr}
45		stp fp, lr, [sp, #-16]!
46		mov fp, sp
47		// Stack pointer unchanged, no stack allocated arguments
48		adrp x8, .L.str0
49		add x8, x8, :lo12:.L.str0
50		// push {x8}
51		stp x8, xzr, [sp, #-16]!
52		// pop {x8}
53		ldp x8, xzr, [sp], #16
54		mov x8, x8
55		mov x0, x8
56		// statement primitives do not return results (but will clobber r0/rax)
57		bl _prints
58		bl _println
59		mov x0, #1
60		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
61		mov sp, fp
62		// pop {fp, lr}
63		ldp fp, lr, [sp], #16
64		ret
65		mov x8, #1
66		cmp x8, #1
67		b.eq .L0
68		// Stack pointer unchanged, no stack allocated arguments
69		adrp x8, .L.str1
70		add x8, x8, :lo12:.L.str1
71		// push {x8}
72		stp x8, xzr, [sp, #-16]!
73		// pop {x8}
74		ldp x8, xzr, [sp], #16
75		mov x8, x8
76		mov x0, x8
77		// statement primitives do not return results (but will clobber r0/rax)
78		bl _prints
79		bl _println
80		mov x0, #3
81		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
82		mov sp, fp
83		// pop {fp, lr}
84		ldp fp, lr, [sp], #16
85		ret
86		b .L1
87	.L0:
88		// Stack pointer unchanged, no stack allocated arguments
89		adrp x8, .L.str2
90		add x8, x8, :lo12:.L.str2
91		// push {x8}
92		stp x8, xzr, [sp, #-16]!
93		// pop {x8}
94		ldp x8, xzr, [sp], #16
95		mov x8, x8
96		mov x0, x8
97		// statement primitives do not return results (but will clobber r0/rax)
98		bl _prints
99		bl _println
100		mov x0, #2
101		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
102		mov sp, fp
103		// pop {fp, lr}
104		ldp fp, lr, [sp], #16
105		ret
106	.L1:
107		// 'ere be dragons: this is 100% dead code, functions always end in returns!
108	
109	// length of .L._prints_str0
110		.word 4
111	.L._prints_str0:
112		.asciz "%.*s"
113	.align 4
114	_prints:
115		// push {lr}
116		stp lr, xzr, [sp, #-16]!
117		mov x2, x0
118		ldrsw x1, [x0, #-4]
119		adr x0, .L._prints_str0
120		bl printf
121		mov x0, #0
122		bl fflush
123		// pop {lr}
124		ldp lr, xzr, [sp], #16
125		ret
126	
127	// length of .L._printi_str0
128		.word 2
129	.L._printi_str0:
130		.asciz "%d"
131	.align 4
132	_printi:
133		// push {lr}
134		stp lr, xzr, [sp, #-16]!
135		mov x1, x0
136		adr x0, .L._printi_str0
137		bl printf
138		mov x0, #0
139		bl fflush
140		// pop {lr}
141		ldp lr, xzr, [sp], #16
142		ret
143	
144	// length of .L._println_str0
145		.word 0
146	.L._println_str0:
147		.asciz ""
148	.align 4
149	_println:
150		// push {lr}
151		stp lr, xzr, [sp, #-16]!
152		adr x0, .L._println_str0
153		bl puts
154		mov x0, #0
155		bl fflush
156		// pop {lr}
157		ldp lr, xzr, [sp], #16
158		ret
===========================================================
-- Finished

