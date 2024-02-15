./valid/scope/scopeRedefine.wacc
calling the reference compiler on ./valid/scope/scopeRedefine.wacc
-- Test: scopeRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# more complex variable scoping test that redefines a variable

# Output:
# true
# 2
#

# Program:

begin
  int x = 1 ;
  begin
    x = 2 ;
    bool x = true ;
    println x
  end ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeRedefine.s contents are:
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
9		mov r8, #1
10		mov r4, r8
11		@ Stack pointer unchanged, no stack allocated variables
12		mov r8, #2
13		mov r4, r8
14		mov r8, #1
15		mov r5, r8
16		@ Stack pointer unchanged, no stack allocated arguments
17		mov r8, r5
18		mov r0, r8
19		@ statement primitives do not return results (but will clobber r0/rax)
20		bl _printb
21		bl _println
22		@ Stack pointer unchanged, no stack allocated variables
23		@ Stack pointer unchanged, no stack allocated arguments
24		mov r8, r4
25		mov r0, r8
26		@ statement primitives do not return results (but will clobber r0/rax)
27		bl _printi
28		bl _println
29		@ Stack pointer unchanged, no stack allocated variables
30		mov r0, #0
31		pop {r4, r5, r8, r10, r12}
32		pop {fp, pc}
33	
34	@ length of .L._printi_str0
35		.word 2
36	.L._printi_str0:
37		.asciz "%d"
38	.align 4
39	_printi:
40		push {fp, lr}
41		mov fp, sp
42		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
43		bic sp, sp, #0x7
44		mov r1, r0
45		adr r0, .L._printi_str0
46		bl printf
47		mov r0, #0
48		bl fflush
49		mov sp, fp
50		pop {fp, pc}
51	
52	@ length of .L._printb_str0
53		.word 5
54	.L._printb_str0:
55		.asciz "false"
56	@ length of .L._printb_str1
57		.word 4
58	.L._printb_str1:
59		.asciz "true"
60	@ length of .L._printb_str2
61		.word 4
62	.L._printb_str2:
63		.asciz "%.*s"
64	.align 4
65	_printb:
66		push {fp, lr}
67		mov fp, sp
68		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
69		bic sp, sp, #0x7
70		cmp r0, #0
71		bne .L_printb0
72		adr r2, .L._printb_str0
73		b .L_printb1
74	.L_printb0:
75		adr r2, .L._printb_str1
76	.L_printb1:
77		ldr r1, [r2, #-4]
78		adr r0, .L._printb_str2
79		bl printf
80		mov r0, #0
81		bl fflush
82		mov sp, fp
83		pop {fp, pc}
84	
85	@ length of .L._println_str0
86		.word 0
87	.L._println_str0:
88		.asciz ""
89	.align 4
90	_println:
91		push {fp, lr}
92		mov fp, sp
93		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
94		bic sp, sp, #0x7
95		adr r0, .L._println_str0
96		bl puts
97		mov r0, #0
98		bl fflush
99		mov sp, fp
100		pop {fp, pc}
===========================================================
-- Finished

