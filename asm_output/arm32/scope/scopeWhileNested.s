./valid/scope/scopeWhileNested.wacc
calling the reference compiler on ./valid/scope/scopeWhileNested.wacc
-- Test: scopeWhileNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping nested within a while-loop

# Output:
# counting... 5
# counting... 4
# counting... 3
# counting... 2
# counting... 1
# 0 Boom!
#

# Program:

begin
  int x = 5 ;
  while x > 0 do
    begin
      string x = "counting... " ;
      print x
    end ;
    println x ;
    x = x - 1
  done ;
  print x ;
  println " Boom!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileNested.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "counting... "
5	@ length of .L.str1
6		.word 6
7	.L.str1:
8		.asciz " Boom!"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r5, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #5
18		mov r4, r8
19		b .L0
20	.L1:
21		@ Stack pointer unchanged, no stack allocated variables
22		ldr r8, =.L.str0
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r5, r8
27		@ Stack pointer unchanged, no stack allocated arguments
28		mov r8, r5
29		mov r0, r8
30		@ statement primitives do not return results (but will clobber r0/rax)
31		bl _prints
32		@ Stack pointer unchanged, no stack allocated variables
33		@ Stack pointer unchanged, no stack allocated arguments
34		mov r8, r4
35		mov r0, r8
36		@ statement primitives do not return results (but will clobber r0/rax)
37		bl _printi
38		bl _println
39		subs r8, r4, #1
40		blvs _errOverflow
41		push {r8}
42		pop {r8}
43		mov r8, r8
44		mov r4, r8
45	.L0:
46		cmp r4, #0
47		bgt .L1
48		@ Stack pointer unchanged, no stack allocated arguments
49		mov r8, r4
50		mov r0, r8
51		@ statement primitives do not return results (but will clobber r0/rax)
52		bl _printi
53		@ Stack pointer unchanged, no stack allocated arguments
54		ldr r8, =.L.str1
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r0, r8
59		@ statement primitives do not return results (but will clobber r0/rax)
60		bl _prints
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
104	@ length of .L._println_str0
105		.word 0
106	.L._println_str0:
107		.asciz ""
108	.align 4
109	_println:
110		push {fp, lr}
111		mov fp, sp
112		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
113		bic sp, sp, #0x7
114		adr r0, .L._println_str0
115		bl puts
116		mov r0, #0
117		bl fflush
118		mov sp, fp
119		pop {fp, pc}
120	
121	_exit:
122		push {fp, lr}
123		mov fp, sp
124		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
125		bic sp, sp, #0x7
126		bl exit
127		mov sp, fp
128		pop {fp, pc}
129	
130	@ length of .L._errOverflow_str0
131		.word 52
132	.L._errOverflow_str0:
133		.asciz "fatal error: integer overflow or underflow occurred\n"
134	.align 4
135	_errOverflow:
136		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
137		bic sp, sp, #0x7
138		adr r0, .L._errOverflow_str0
139		bl _prints
140		mov r0, #255
141		bl _exit
===========================================================
-- Finished

