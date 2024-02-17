./valid/function/simple_functions/functionMultiReturns.wacc
calling the reference compiler on ./valid/function/simple_functions/functionMultiReturns.wacc
-- Test: functionMultiReturns.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple function with multiple returns, importantly one at the end.

# Output:
# 1
#

# Program:

begin
  int returnInWhile() is
    while true do
      return 1 ;
      println "How on Earth did we get here?"
    done ;
    return 2
  end

  int x = call returnInWhile() ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionMultiReturns.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 29
3	.L.str0:
4		.asciz "How on Earth did we get here?"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		@ Stack pointer unchanged, no stack allocated arguments
14		bl wacc_returnInWhile
15		mov r12, r0
16		@ Stack pointer unchanged, no stack allocated arguments
17		mov r8, r12
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, r4
21		mov r0, r8
22		@ statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		@ Stack pointer unchanged, no stack allocated variables
26		mov r0, #0
27		pop {r4, r8, r10, r12}
28		pop {fp, pc}
29	
30	wacc_returnInWhile:
31		push {fp, lr}
32		mov fp, sp
33		b .L0
34	.L1:
35		mov r0, #1
36		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
37		mov sp, fp
38		pop {fp, pc}
39		@ Stack pointer unchanged, no stack allocated arguments
40		ldr r8, =.L.str0
41		push {r8}
42		pop {r8}
43		mov r8, r8
44		mov r0, r8
45		@ statement primitives do not return results (but will clobber r0/rax)
46		bl _prints
47		bl _println
48	.L0:
49		mov r8, #1
50		cmp r8, #1
51		beq .L1
52		mov r0, #2
53		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
54		mov sp, fp
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
80	@ length of .L._printi_str0
81		.word 2
82	.L._printi_str0:
83		.asciz "%d"
84	.align 4
85	_printi:
86		push {fp, lr}
87		mov fp, sp
88		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
89		bic sp, sp, #0x7
90		mov r1, r0
91		adr r0, .L._printi_str0
92		bl printf
93		mov r0, #0
94		bl fflush
95		mov sp, fp
96		pop {fp, pc}
97	
98	@ length of .L._println_str0
99		.word 0
100	.L._println_str0:
101		.asciz ""
102	.align 4
103	_println:
104		push {fp, lr}
105		mov fp, sp
106		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
107		bic sp, sp, #0x7
108		adr r0, .L._println_str0
109		bl puts
110		mov r0, #0
111		bl fflush
112		mov sp, fp
113		pop {fp, pc}
===========================================================
-- Finished

