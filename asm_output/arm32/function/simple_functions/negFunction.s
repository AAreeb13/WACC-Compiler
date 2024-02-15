./valid/function/simple_functions/negFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/negFunction.wacc
-- Test: negFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple negation function definition and usage

# Output:
# true
# false
# true
#

# Program:

begin
  bool neg(bool b) is
    return !b
  end
  bool b = true ;
  println b ;
  b = call neg(b) ;
  println b ;
  b = call neg(b) ;
  b = call neg(b) ;
  b = call neg(b) ;
  println b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negFunction.s contents are:
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
9		mov r8, #1
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, r4
13		mov r0, r8
14		@ statement primitives do not return results (but will clobber r0/rax)
15		bl _printb
16		bl _println
17		@ Stack pointer unchanged, no stack allocated arguments
18		mov r8, r4
19		mov r0, r8
20		bl wacc_neg
21		mov r12, r0
22		@ Stack pointer unchanged, no stack allocated arguments
23		mov r8, r12
24		mov r4, r8
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r4
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _printb
30		bl _println
31		@ Stack pointer unchanged, no stack allocated arguments
32		mov r8, r4
33		mov r0, r8
34		bl wacc_neg
35		mov r12, r0
36		@ Stack pointer unchanged, no stack allocated arguments
37		mov r8, r12
38		mov r4, r8
39		@ Stack pointer unchanged, no stack allocated arguments
40		mov r8, r4
41		mov r0, r8
42		bl wacc_neg
43		mov r12, r0
44		@ Stack pointer unchanged, no stack allocated arguments
45		mov r8, r12
46		mov r4, r8
47		@ Stack pointer unchanged, no stack allocated arguments
48		mov r8, r4
49		mov r0, r8
50		bl wacc_neg
51		mov r12, r0
52		@ Stack pointer unchanged, no stack allocated arguments
53		mov r8, r12
54		mov r4, r8
55		@ Stack pointer unchanged, no stack allocated arguments
56		mov r8, r4
57		mov r0, r8
58		@ statement primitives do not return results (but will clobber r0/rax)
59		bl _printb
60		bl _println
61		@ Stack pointer unchanged, no stack allocated variables
62		mov r0, #0
63		pop {r4, r8, r10, r12}
64		pop {fp, pc}
65	
66	wacc_neg:
67		push {fp, lr}
68		mov fp, sp
69		cmp r0, #1
70		movne r8, #1
71		moveq r8, #0
72		push {r8}
73		pop {r0}
74		mov r0, r0
75		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
76		mov sp, fp
77		pop {fp, pc}
78		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
79		@ the .ltorg here is used to generate the constant pool after every function
80		@ this is important for ensuring string literals and large integers are accessible!
81		.ltorg
82	
83	@ length of .L._printb_str0
84		.word 5
85	.L._printb_str0:
86		.asciz "false"
87	@ length of .L._printb_str1
88		.word 4
89	.L._printb_str1:
90		.asciz "true"
91	@ length of .L._printb_str2
92		.word 4
93	.L._printb_str2:
94		.asciz "%.*s"
95	.align 4
96	_printb:
97		push {fp, lr}
98		mov fp, sp
99		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
100		bic sp, sp, #0x7
101		cmp r0, #0
102		bne .L_printb0
103		adr r2, .L._printb_str0
104		b .L_printb1
105	.L_printb0:
106		adr r2, .L._printb_str1
107	.L_printb1:
108		ldr r1, [r2, #-4]
109		adr r0, .L._printb_str2
110		bl printf
111		mov r0, #0
112		bl fflush
113		mov sp, fp
114		pop {fp, pc}
115	
116	@ length of .L._println_str0
117		.word 0
118	.L._println_str0:
119		.asciz ""
120	.align 4
121	_println:
122		push {fp, lr}
123		mov fp, sp
124		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
125		bic sp, sp, #0x7
126		adr r0, .L._println_str0
127		bl puts
128		mov r0, #0
129		bl fflush
130		mov sp, fp
131		pop {fp, pc}
===========================================================
-- Finished

