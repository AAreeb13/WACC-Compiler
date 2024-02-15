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
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		@ Stack padded to a multiple of the required alignment
10		sub sp, sp, #12
11		mov r8, #1
12		mov r0, r8
13		mov r8, #2
14		mov r1, r8
15		mov r8, #3
16		mov r2, r8
17		mov r8, #4
18		mov r3, r8
19		mov r8, #5
20		str r8, [sp, #4]
21		mov r8, #6
22		str r8, [sp, #0]
23		bl wacc_f
24		mov r12, r0
25		@ Stack padded to a multiple of the required alignment
26		add sp, sp, #12
27		mov r8, r12
28		mov r4, r8
29		@ Stack pointer unchanged, no stack allocated arguments
30		mov r8, r4
31		mov r0, r8
32		@ statement primitives do not return results (but will clobber r0/rax)
33		bl _printi
34		bl _println
35		@ Stack pointer unchanged, no stack allocated variables
36		mov r0, #0
37		pop {r4, r8, r10, r12}
38		pop {fp, pc}
39	
40	wacc_f:
41		push {fp, lr}
42		push {r4, r5, r6, r7}
43		mov fp, sp
44		@ Stack padded to a multiple of the required alignment
45		sub sp, sp, #8
46		mov r8, #1
47		mov r4, r8
48		mov r8, #2
49		mov r5, r8
50		mov r8, #3
51		mov r6, r8
52		mov r8, #4
53		mov r7, r8
54		mov r8, #53
55		strb r8, [fp, #-7]
56		mov r8, #54
57		strb r8, [fp, #-6]
58		mov r8, #55
59		strb r8, [fp, #-5]
60		mov r8, #8
61		str r8, [fp, #-4]
62		@ Stack padded to a multiple of the required alignment
63		sub sp, sp, #8
64		mov r8, #97
65		strb r8, [fp, #-13]
66		mov r8, #98
67		strb r8, [fp, #-12]
68		mov r8, #5
69		str r8, [fp, #-11]
70		push {r0, r1, r2, r3}
71		@ Set up R12 as a temporary second base pointer for the caller saved things
72		mov r12, sp
73		@ Stack pointer unchanged, no stack allocated arguments
74		ldr r8, [fp, #-11]
75		mov r8, r8
76		mov r0, r8
77		@ statement primitives do not return results (but will clobber r0/rax)
78		bl _printi
79		bl _println
80		pop {r0, r1, r2, r3}
81		@ Stack padded to a multiple of the required alignment
82		add sp, sp, #8
83		ldr r0, [fp, #-4]
84		mov r0, r0
85		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
86		mov sp, fp
87		pop {r4, r5, r6, r7}
88		pop {fp, pc}
89		@ Stack padded to a multiple of the required alignment
90		add sp, sp, #8
91		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
92		@ the .ltorg here is used to generate the constant pool after every function
93		@ this is important for ensuring string literals and large integers are accessible!
94		.ltorg
95	
96	@ length of .L._printi_str0
97		.word 2
98	.L._printi_str0:
99		.asciz "%d"
100	.align 4
101	_printi:
102		push {fp, lr}
103		mov fp, sp
104		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
105		bic sp, sp, #0x7
106		mov r1, r0
107		adr r0, .L._printi_str0
108		bl printf
109		mov r0, #0
110		bl fflush
111		mov sp, fp
112		pop {fp, pc}
113	
114	@ length of .L._println_str0
115		.word 0
116	.L._println_str0:
117		.asciz ""
118	.align 4
119	_println:
120		push {fp, lr}
121		mov fp, sp
122		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
123		bic sp, sp, #0x7
124		adr r0, .L._println_str0
125		bl puts
126		mov r0, #0
127		bl fflush
128		mov sp, fp
129		pop {fp, pc}
===========================================================
-- Finished

