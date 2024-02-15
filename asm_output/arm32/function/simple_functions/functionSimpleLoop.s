./valid/function/simple_functions/functionSimpleLoop.wacc
calling the reference compiler on ./valid/function/simple_functions/functionSimpleLoop.wacc
-- Test: functionSimpleLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# define and call a function with a simple loop

# Output:
# 10
#

# Program:

begin
  int f(int n) is
    int i = 0;
    while i < n do
      i = i + 1
    done;
    return i
  end
  int x = call f(10) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionSimpleLoop.s contents are:
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
9		@ Stack pointer unchanged, no stack allocated arguments
10		mov r8, #10
11		mov r0, r8
12		bl wacc_f
13		mov r12, r0
14		@ Stack pointer unchanged, no stack allocated arguments
15		mov r8, r12
16		mov r4, r8
17		@ Stack pointer unchanged, no stack allocated arguments
18		mov r8, r4
19		mov r0, r8
20		@ statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		@ Stack pointer unchanged, no stack allocated variables
24		mov r0, #0
25		pop {r4, r8, r10, r12}
26		pop {fp, pc}
27	
28	wacc_f:
29		push {fp, lr}
30		push {r4}
31		mov fp, sp
32		@ Stack pointer unchanged, no stack allocated variables
33		mov r8, #0
34		mov r4, r8
35		b .L0
36	.L1:
37		adds r8, r4, #1
38		blvs _errOverflow
39		push {r8}
40		pop {r8}
41		mov r8, r8
42		mov r4, r8
43	.L0:
44		cmp r4, r0
45		blt .L1
46		mov r0, r4
47		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
48		mov sp, fp
49		pop {r4}
50		pop {fp, pc}
51		@ Stack pointer unchanged, no stack allocated variables
52		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
53		@ the .ltorg here is used to generate the constant pool after every function
54		@ this is important for ensuring string literals and large integers are accessible!
55		.ltorg
56	
57	@ length of .L._prints_str0
58		.word 4
59	.L._prints_str0:
60		.asciz "%.*s"
61	.align 4
62	_prints:
63		push {fp, lr}
64		mov fp, sp
65		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
66		bic sp, sp, #0x7
67		mov r2, r0
68		ldr r1, [r0, #-4]
69		adr r0, .L._prints_str0
70		bl printf
71		mov r0, #0
72		bl fflush
73		mov sp, fp
74		pop {fp, pc}
75	
76	@ length of .L._printi_str0
77		.word 2
78	.L._printi_str0:
79		.asciz "%d"
80	.align 4
81	_printi:
82		push {fp, lr}
83		mov fp, sp
84		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
85		bic sp, sp, #0x7
86		mov r1, r0
87		adr r0, .L._printi_str0
88		bl printf
89		mov r0, #0
90		bl fflush
91		mov sp, fp
92		pop {fp, pc}
93	
94	@ length of .L._println_str0
95		.word 0
96	.L._println_str0:
97		.asciz ""
98	.align 4
99	_println:
100		push {fp, lr}
101		mov fp, sp
102		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
103		bic sp, sp, #0x7
104		adr r0, .L._println_str0
105		bl puts
106		mov r0, #0
107		bl fflush
108		mov sp, fp
109		pop {fp, pc}
110	
111	_exit:
112		push {fp, lr}
113		mov fp, sp
114		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
115		bic sp, sp, #0x7
116		bl exit
117		mov sp, fp
118		pop {fp, pc}
119	
120	@ length of .L._errOverflow_str0
121		.word 52
122	.L._errOverflow_str0:
123		.asciz "fatal error: integer overflow or underflow occurred\n"
124	.align 4
125	_errOverflow:
126		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
127		bic sp, sp, #0x7
128		adr r0, .L._errOverflow_str0
129		bl _prints
130		mov r0, #255
131		bl _exit
===========================================================
-- Finished

