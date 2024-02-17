./valid/scope/scopeIfRedefine.wacc
calling the reference compiler on ./valid/scope/scopeIfRedefine.wacc
-- Test: scopeIfRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within an if-statement

# Output:
# true
# 12
#

# Program:

begin
  int x = 12 ;
  if x == 12 then
    bool x = true ;
    println x
  else
    char x = 'a';
    println x
  fi ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeIfRedefine.s contents are:
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
11		cmp r4, #12
12		beq .L0
13		@ Stack pointer unchanged, no stack allocated variables
14		mov r8, #97
15		mov r5, r8
16		@ Stack pointer unchanged, no stack allocated arguments
17		mov r8, r5
18		mov r0, r8
19		@ statement primitives do not return results (but will clobber r0/rax)
20		bl _printc
21		bl _println
22		@ Stack pointer unchanged, no stack allocated variables
23		b .L1
24	.L0:
25		@ Stack pointer unchanged, no stack allocated variables
26		mov r8, #1
27		mov r5, r8
28		@ Stack pointer unchanged, no stack allocated arguments
29		mov r8, r5
30		mov r0, r8
31		@ statement primitives do not return results (but will clobber r0/rax)
32		bl _printb
33		bl _println
34		@ Stack pointer unchanged, no stack allocated variables
35	.L1:
36		@ Stack pointer unchanged, no stack allocated arguments
37		mov r8, r4
38		mov r0, r8
39		@ statement primitives do not return results (but will clobber r0/rax)
40		bl _printi
41		bl _println
42		@ Stack pointer unchanged, no stack allocated variables
43		mov r0, #0
44		pop {r4, r5, r8, r10, r12}
45		pop {fp, pc}
46	
47	@ length of .L._printi_str0
48		.word 2
49	.L._printi_str0:
50		.asciz "%d"
51	.align 4
52	_printi:
53		push {fp, lr}
54		mov fp, sp
55		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
56		bic sp, sp, #0x7
57		mov r1, r0
58		adr r0, .L._printi_str0
59		bl printf
60		mov r0, #0
61		bl fflush
62		mov sp, fp
63		pop {fp, pc}
64	
65	@ length of .L._printb_str0
66		.word 5
67	.L._printb_str0:
68		.asciz "false"
69	@ length of .L._printb_str1
70		.word 4
71	.L._printb_str1:
72		.asciz "true"
73	@ length of .L._printb_str2
74		.word 4
75	.L._printb_str2:
76		.asciz "%.*s"
77	.align 4
78	_printb:
79		push {fp, lr}
80		mov fp, sp
81		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
82		bic sp, sp, #0x7
83		cmp r0, #0
84		bne .L_printb0
85		adr r2, .L._printb_str0
86		b .L_printb1
87	.L_printb0:
88		adr r2, .L._printb_str1
89	.L_printb1:
90		ldr r1, [r2, #-4]
91		adr r0, .L._printb_str2
92		bl printf
93		mov r0, #0
94		bl fflush
95		mov sp, fp
96		pop {fp, pc}
97	
98	@ length of .L._printc_str0
99		.word 2
100	.L._printc_str0:
101		.asciz "%c"
102	.align 4
103	_printc:
104		push {fp, lr}
105		mov fp, sp
106		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
107		bic sp, sp, #0x7
108		mov r1, r0
109		adr r0, .L._printc_str0
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

