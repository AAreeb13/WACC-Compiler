./valid/IO/print/printCharAsString.wacc
calling the reference compiler on ./valid/IO/print/printCharAsString.wacc
-- Test: printCharAsString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# character array treated as a string

# Output:
# foo
# bar
#

# Program:

begin
  char[] str = ['f','o','o'];
  println str;
  str = ['b','a','r'];
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharAsString.s contents are:
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
9		@ 3 element array
10		mov r0, #7
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #3
16		str r8, [r12, #-4]
17		mov r8, #102
18		strb r8, [r12, #0]
19		mov r8, #111
20		strb r8, [r12, #1]
21		mov r8, #111
22		strb r8, [r12, #2]
23		mov r8, r12
24		mov r4, r8
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r4
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _prints
30		bl _println
31		@ 3 element array
32		mov r0, #7
33		bl _malloc
34		mov r12, r0
35		@ array pointers are shifted forwards by 4 bytes (to account for size)
36		add r12, r12, #4
37		mov r8, #3
38		str r8, [r12, #-4]
39		mov r8, #98
40		strb r8, [r12, #0]
41		mov r8, #97
42		strb r8, [r12, #1]
43		mov r8, #114
44		strb r8, [r12, #2]
45		mov r8, r12
46		mov r4, r8
47		@ Stack pointer unchanged, no stack allocated arguments
48		mov r8, r4
49		mov r0, r8
50		@ statement primitives do not return results (but will clobber r0/rax)
51		bl _prints
52		bl _println
53		@ Stack pointer unchanged, no stack allocated variables
54		mov r0, #0
55		pop {r4, r8, r10, r12}
56		pop {fp, pc}
57	
58	@ length of .L._prints_str0
59		.word 4
60	.L._prints_str0:
61		.asciz "%.*s"
62	.align 4
63	_prints:
64		push {fp, lr}
65		mov fp, sp
66		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
67		bic sp, sp, #0x7
68		mov r2, r0
69		ldr r1, [r0, #-4]
70		adr r0, .L._prints_str0
71		bl printf
72		mov r0, #0
73		bl fflush
74		mov sp, fp
75		pop {fp, pc}
76	
77	_malloc:
78		push {fp, lr}
79		mov fp, sp
80		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
81		bic sp, sp, #0x7
82		bl malloc
83		cmp r0, #0
84		bleq _errOutOfMemory
85		mov sp, fp
86		pop {fp, pc}
87	
88	@ length of .L._println_str0
89		.word 0
90	.L._println_str0:
91		.asciz ""
92	.align 4
93	_println:
94		push {fp, lr}
95		mov fp, sp
96		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
97		bic sp, sp, #0x7
98		adr r0, .L._println_str0
99		bl puts
100		mov r0, #0
101		bl fflush
102		mov sp, fp
103		pop {fp, pc}
104	
105	_exit:
106		push {fp, lr}
107		mov fp, sp
108		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
109		bic sp, sp, #0x7
110		bl exit
111		mov sp, fp
112		pop {fp, pc}
113	
114	@ length of .L._errOutOfMemory_str0
115		.word 27
116	.L._errOutOfMemory_str0:
117		.asciz "fatal error: out of memory\n"
118	.align 4
119	_errOutOfMemory:
120		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
121		bic sp, sp, #0x7
122		adr r0, .L._errOutOfMemory_str0
123		bl _prints
124		mov r0, #255
125		bl _exit
===========================================================
-- Finished

