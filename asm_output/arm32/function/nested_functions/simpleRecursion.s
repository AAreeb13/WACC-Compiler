./valid/function/nested_functions/simpleRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/simpleRecursion.wacc
-- Test: simpleRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple recursive function

# Output:

# Program:

begin
  int rec(int x) is
    if x == 0
    then
      skip
    else
      int y = call rec(x - 1)
    fi ;
    return 42
  end

  int x = 0 ;
  x = call rec(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
simpleRecursion.s contents are:
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
9		mov r8, #0
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated arguments
12		mov r8, #8
13		mov r0, r8
14		bl wacc_rec
15		mov r12, r0
16		@ Stack pointer unchanged, no stack allocated arguments
17		mov r8, r12
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated variables
20		mov r0, #0
21		pop {r4, r8, r10, r12}
22		pop {fp, pc}
23	
24	wacc_rec:
25		push {fp, lr}
26		push {r4}
27		mov fp, sp
28		cmp r0, #0
29		beq .L0
30		@ Stack pointer unchanged, no stack allocated variables
31		push {r0}
32		@ Set up R12 as a temporary second base pointer for the caller saved things
33		mov r12, sp
34		@ Stack pointer unchanged, no stack allocated arguments
35		subs r8, r0, #1
36		blvs _errOverflow
37		push {r8}
38		pop {r8}
39		mov r8, r8
40		mov r0, r8
41		bl wacc_rec
42		mov r12, r0
43		@ Stack pointer unchanged, no stack allocated arguments
44		pop {r0}
45		mov r8, r12
46		mov r4, r8
47		@ Stack pointer unchanged, no stack allocated variables
48		b .L1
49	.L0:
50	.L1:
51		mov r0, #42
52		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
53		mov sp, fp
54		pop {r4}
55		pop {fp, pc}
56		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
57		@ the .ltorg here is used to generate the constant pool after every function
58		@ this is important for ensuring string literals and large integers are accessible!
59		.ltorg
60	
61	@ length of .L._prints_str0
62		.word 4
63	.L._prints_str0:
64		.asciz "%.*s"
65	.align 4
66	_prints:
67		push {fp, lr}
68		mov fp, sp
69		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
70		bic sp, sp, #0x7
71		mov r2, r0
72		ldr r1, [r0, #-4]
73		adr r0, .L._prints_str0
74		bl printf
75		mov r0, #0
76		bl fflush
77		mov sp, fp
78		pop {fp, pc}
79	
80	_exit:
81		push {fp, lr}
82		mov fp, sp
83		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
84		bic sp, sp, #0x7
85		bl exit
86		mov sp, fp
87		pop {fp, pc}
88	
89	@ length of .L._errOverflow_str0
90		.word 52
91	.L._errOverflow_str0:
92		.asciz "fatal error: integer overflow or underflow occurred\n"
93	.align 4
94	_errOverflow:
95		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
96		bic sp, sp, #0x7
97		adr r0, .L._errOverflow_str0
98		bl _prints
99		mov r0, #255
100		bl _exit
===========================================================
-- Finished

