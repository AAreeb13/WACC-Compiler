./valid/pairs/writeSnd.wacc
calling the reference compiler on ./valid/pairs/writeSnd.wacc
-- Test: writeSnd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair and write to its second element

# Output:
# a
# Z
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  char s = snd p ;
  println s ;
  snd p = 'Z' ;
  s = snd p ;
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
writeSnd.s contents are:
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
9		mov r0, #8
10		bl _malloc
11		mov r12, r0
12		mov r8, #10
13		str r8, [r12, #0]
14		mov r8, #97
15		str r8, [r12, #4]
16		mov r8, r12
17		mov r4, r8
18		cmp r4, #0
19		bleq _errNull
20		ldr r8, [r4, #4]
21		mov r8, r8
22		mov r5, r8
23		@ Stack pointer unchanged, no stack allocated arguments
24		mov r8, r5
25		mov r0, r8
26		@ statement primitives do not return results (but will clobber r0/rax)
27		bl _printc
28		bl _println
29		cmp r4, #0
30		bleq _errNull
31		mov r8, #90
32		str r8, [r4, #4]
33		cmp r4, #0
34		bleq _errNull
35		ldr r8, [r4, #4]
36		mov r8, r8
37		push {r8}
38		pop {r8}
39		mov r8, r8
40		mov r5, r8
41		@ Stack pointer unchanged, no stack allocated arguments
42		mov r8, r5
43		mov r0, r8
44		@ statement primitives do not return results (but will clobber r0/rax)
45		bl _printc
46		bl _println
47		@ Stack pointer unchanged, no stack allocated variables
48		mov r0, #0
49		pop {r4, r5, r8, r10, r12}
50		pop {fp, pc}
51	
52	@ length of .L._prints_str0
53		.word 4
54	.L._prints_str0:
55		.asciz "%.*s"
56	.align 4
57	_prints:
58		push {fp, lr}
59		mov fp, sp
60		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
61		bic sp, sp, #0x7
62		mov r2, r0
63		ldr r1, [r0, #-4]
64		adr r0, .L._prints_str0
65		bl printf
66		mov r0, #0
67		bl fflush
68		mov sp, fp
69		pop {fp, pc}
70	
71	_malloc:
72		push {fp, lr}
73		mov fp, sp
74		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
75		bic sp, sp, #0x7
76		bl malloc
77		cmp r0, #0
78		bleq _errOutOfMemory
79		mov sp, fp
80		pop {fp, pc}
81	
82	@ length of .L._printc_str0
83		.word 2
84	.L._printc_str0:
85		.asciz "%c"
86	.align 4
87	_printc:
88		push {fp, lr}
89		mov fp, sp
90		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
91		bic sp, sp, #0x7
92		mov r1, r0
93		adr r0, .L._printc_str0
94		bl printf
95		mov r0, #0
96		bl fflush
97		mov sp, fp
98		pop {fp, pc}
99	
100	@ length of .L._println_str0
101		.word 0
102	.L._println_str0:
103		.asciz ""
104	.align 4
105	_println:
106		push {fp, lr}
107		mov fp, sp
108		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
109		bic sp, sp, #0x7
110		adr r0, .L._println_str0
111		bl puts
112		mov r0, #0
113		bl fflush
114		mov sp, fp
115		pop {fp, pc}
116	
117	_exit:
118		push {fp, lr}
119		mov fp, sp
120		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
121		bic sp, sp, #0x7
122		bl exit
123		mov sp, fp
124		pop {fp, pc}
125	
126	@ length of .L._errOutOfMemory_str0
127		.word 27
128	.L._errOutOfMemory_str0:
129		.asciz "fatal error: out of memory\n"
130	.align 4
131	_errOutOfMemory:
132		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
133		bic sp, sp, #0x7
134		adr r0, .L._errOutOfMemory_str0
135		bl _prints
136		mov r0, #255
137		bl _exit
138	
139	@ length of .L._errNull_str0
140		.word 45
141	.L._errNull_str0:
142		.asciz "fatal error: null pair dereferenced or freed\n"
143	.align 4
144	_errNull:
145		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
146		bic sp, sp, #0x7
147		adr r0, .L._errNull_str0
148		bl _prints
149		mov r0, #255
150		bl _exit
===========================================================
-- Finished

