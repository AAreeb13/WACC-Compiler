./valid/expressions/ordAndchrExpr.wacc
calling the reference compiler on ./valid/expressions/ordAndchrExpr.wacc
-- Test: ordAndchrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evalutaing ord and chr

# Output:
# a is 97
# 99 is c
#

# Program:

begin
  char a = 'a' ;
  int i = 99 ;

  print a ;
  print " is " ;
  println ord a ;

  print i ;
  print " is " ;
  println chr i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ordAndchrExpr.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " is "
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r5, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r8, #97
14		mov r4, r8
15		mov r8, #99
16		mov r5, r8
17		@ Stack pointer unchanged, no stack allocated arguments
18		mov r8, r4
19		mov r0, r8
20		@ statement primitives do not return results (but will clobber r0/rax)
21		bl _printc
22		@ Stack pointer unchanged, no stack allocated arguments
23		ldr r8, =.L.str0
24		push {r8}
25		pop {r8}
26		mov r8, r8
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _prints
30		@ Stack pointer unchanged, no stack allocated arguments
31		mov r8, r4
32		mov r0, r8
33		@ statement primitives do not return results (but will clobber r0/rax)
34		bl _printi
35		bl _println
36		@ Stack pointer unchanged, no stack allocated arguments
37		mov r8, r5
38		mov r0, r8
39		@ statement primitives do not return results (but will clobber r0/rax)
40		bl _printi
41		@ Stack pointer unchanged, no stack allocated arguments
42		ldr r8, =.L.str0
43		push {r8}
44		pop {r8}
45		mov r8, r8
46		mov r0, r8
47		@ statement primitives do not return results (but will clobber r0/rax)
48		bl _prints
49		@ Stack pointer unchanged, no stack allocated arguments
50		mov r8, r5
51		ldr r9, =-128
52		tst r8, r9
53		movne r1, r8
54		blne _errBadChar
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r0, r8
59		@ statement primitives do not return results (but will clobber r0/rax)
60		bl _printc
61		bl _println
62		@ Stack pointer unchanged, no stack allocated variables
63		mov r0, #0
64		pop {r4, r5, r8, r10, r12}
65		pop {fp, pc}
66	
67	@ length of .L._prints_str0
68		.word 4
69	.L._prints_str0:
70		.asciz "%.*s"
71	.align 4
72	_prints:
73		push {fp, lr}
74		mov fp, sp
75		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
76		bic sp, sp, #0x7
77		mov r2, r0
78		ldr r1, [r0, #-4]
79		adr r0, .L._prints_str0
80		bl printf
81		mov r0, #0
82		bl fflush
83		mov sp, fp
84		pop {fp, pc}
85	
86	@ length of .L._printi_str0
87		.word 2
88	.L._printi_str0:
89		.asciz "%d"
90	.align 4
91	_printi:
92		push {fp, lr}
93		mov fp, sp
94		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
95		bic sp, sp, #0x7
96		mov r1, r0
97		adr r0, .L._printi_str0
98		bl printf
99		mov r0, #0
100		bl fflush
101		mov sp, fp
102		pop {fp, pc}
103	
104	@ length of .L._printc_str0
105		.word 2
106	.L._printc_str0:
107		.asciz "%c"
108	.align 4
109	_printc:
110		push {fp, lr}
111		mov fp, sp
112		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
113		bic sp, sp, #0x7
114		mov r1, r0
115		adr r0, .L._printc_str0
116		bl printf
117		mov r0, #0
118		bl fflush
119		mov sp, fp
120		pop {fp, pc}
121	
122	@ length of .L._println_str0
123		.word 0
124	.L._println_str0:
125		.asciz ""
126	.align 4
127	_println:
128		push {fp, lr}
129		mov fp, sp
130		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
131		bic sp, sp, #0x7
132		adr r0, .L._println_str0
133		bl puts
134		mov r0, #0
135		bl fflush
136		mov sp, fp
137		pop {fp, pc}
138	
139	_exit:
140		push {fp, lr}
141		mov fp, sp
142		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
143		bic sp, sp, #0x7
144		bl exit
145		mov sp, fp
146		pop {fp, pc}
147	
148	@ length of .L._errBadChar_str0
149		.word 50
150	.L._errBadChar_str0:
151		.asciz "fatal error: int %d is not ascii character 0-127 \n"
152	.align 4
153	_errBadChar:
154		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
155		bic sp, sp, #0x7
156		adr r0, .L._errBadChar_str0
157		bl printf
158		mov r0, #0
159		bl fflush
160		mov r0, #255
161		bl _exit
===========================================================
-- Finished

