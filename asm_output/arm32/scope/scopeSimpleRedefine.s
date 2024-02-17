./valid/scope/scopeSimpleRedefine.wacc
calling the reference compiler on ./valid/scope/scopeSimpleRedefine.wacc
-- Test: scopeSimpleRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  begin
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeSimpleRedefine.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #12
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated variables
12		mov r8, #1
13		mov r5, r8
14		@ Stack pointer unchanged, no stack allocated arguments
15		mov r8, r5
16		mov r0, r8
17		@ statement primitives do not return results (but will clobber r0/rax)
18		bl _printb
19		bl _println
20		@ Stack pointer unchanged, no stack allocated variables
21		@ Stack pointer unchanged, no stack allocated arguments
22		mov r8, r4
23		mov r0, r8
24		@ statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		@ Stack pointer unchanged, no stack allocated variables
28		mov r0, #0
29		pop {r4, r5, r8, r10, r12}
30		pop {fp, pc}
31	
32	@ length of .L._printi_str0
33		.word 2
34	.L._printi_str0:
35		.asciz "%d"
36	.align 4
37	_printi:
38		push {fp, lr}
39		mov fp, sp
40		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
41		bic sp, sp, #0x7
42		mov r1, r0
43		adr r0, .L._printi_str0
44		bl printf
45		mov r0, #0
46		bl fflush
47		mov sp, fp
48		pop {fp, pc}
49	
50	@ length of .L._printb_str0
51		.word 5
52	.L._printb_str0:
53		.asciz "false"
54	@ length of .L._printb_str1
55		.word 4
56	.L._printb_str1:
57		.asciz "true"
58	@ length of .L._printb_str2
59		.word 4
60	.L._printb_str2:
61		.asciz "%.*s"
62	.align 4
63	_printb:
64		push {fp, lr}
65		mov fp, sp
66		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
67		bic sp, sp, #0x7
68		cmp r0, #0
69		bne .L_printb0
70		adr r2, .L._printb_str0
71		b .L_printb1
72	.L_printb0:
73		adr r2, .L._printb_str1
74	.L_printb1:
75		ldr r1, [r2, #-4]
76		adr r0, .L._printb_str2
77		bl printf
78		mov r0, #0
79		bl fflush
80		mov sp, fp
81		pop {fp, pc}
82	
83	@ length of .L._println_str0
84		.word 0
85	.L._println_str0:
86		.asciz ""
87	.align 4
88	_println:
89		push {fp, lr}
90		mov fp, sp
91		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
92		bic sp, sp, #0x7
93		adr r0, .L._println_str0
94		bl puts
95		mov r0, #0
96		bl fflush
97		mov sp, fp
98		pop {fp, pc}
===========================================================
-- Finished

