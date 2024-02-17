./valid/IO/IOLoop.wacc
calling the reference compiler on ./valid/IO/IOLoop.wacc
-- Test: IOLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple input/output loop

# Input: 1 Y 2 Y 3 Y 4 Y 5 Y 142 N

# Output:
# Please input an integer: echo input: 1
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 2
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 3
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 4
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 5
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
# Please input an integer: echo input: 142
# Do you want to continue entering input?
# (enter Y for 'yes' and N for 'no')
#

# Program:

begin
  char continue = 'Y' ;
  int buff = 0 ;
  while continue != 'N' do
    print "Please input an integer: " ;
    read buff ;
    print "echo input: " ;
    println buff ;
    println "Do you want to continue entering input?" ;
    println "(enter Y for \'yes\' and N for \'no\')" ;
    read continue
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOLoop.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 25
3	.L.str0:
4		.asciz "Please input an integer: "
5	@ length of .L.str1
6		.word 12
7	.L.str1:
8		.asciz "echo input: "
9	@ length of .L.str2
10		.word 39
11	.L.str2:
12		.asciz "Do you want to continue entering input?"
13	@ length of .L.str3
14		.word 34
15	.L.str3:
16		.asciz "(enter Y for 'yes' and N for 'no')"
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r5, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		mov r8, #89
26		mov r4, r8
27		mov r8, #0
28		mov r5, r8
29		b .L0
30	.L1:
31		@ Stack pointer unchanged, no stack allocated arguments
32		ldr r8, =.L.str0
33		push {r8}
34		pop {r8}
35		mov r8, r8
36		mov r0, r8
37		@ statement primitives do not return results (but will clobber r0/rax)
38		bl _prints
39		@ Stack pointer unchanged, no stack allocated arguments
40		@ load the current value in the destination of the read so it supports defaults
41		mov r8, r5
42		mov r0, r8
43		bl _readi
44		mov r12, r0
45		mov r8, r12
46		mov r5, r8
47		@ Stack pointer unchanged, no stack allocated arguments
48		ldr r8, =.L.str1
49		push {r8}
50		pop {r8}
51		mov r8, r8
52		mov r0, r8
53		@ statement primitives do not return results (but will clobber r0/rax)
54		bl _prints
55		@ Stack pointer unchanged, no stack allocated arguments
56		mov r8, r5
57		mov r0, r8
58		@ statement primitives do not return results (but will clobber r0/rax)
59		bl _printi
60		bl _println
61		@ Stack pointer unchanged, no stack allocated arguments
62		ldr r8, =.L.str2
63		push {r8}
64		pop {r8}
65		mov r8, r8
66		mov r0, r8
67		@ statement primitives do not return results (but will clobber r0/rax)
68		bl _prints
69		bl _println
70		@ Stack pointer unchanged, no stack allocated arguments
71		ldr r8, =.L.str3
72		push {r8}
73		pop {r8}
74		mov r8, r8
75		mov r0, r8
76		@ statement primitives do not return results (but will clobber r0/rax)
77		bl _prints
78		bl _println
79		@ Stack pointer unchanged, no stack allocated arguments
80		@ load the current value in the destination of the read so it supports defaults
81		mov r8, r4
82		mov r0, r8
83		bl _readc
84		mov r12, r0
85		mov r8, r12
86		mov r4, r8
87	.L0:
88		cmp r4, #78
89		bne .L1
90		@ Stack pointer unchanged, no stack allocated variables
91		mov r0, #0
92		pop {r4, r5, r8, r10, r12}
93		pop {fp, pc}
94	
95	@ length of .L._readc_str0
96		.word 3
97	.L._readc_str0:
98		.asciz " %c"
99	.align 4
100	_readc:
101		push {fp, lr}
102		mov fp, sp
103		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
104		bic sp, sp, #0x7
105		@ R0 contains the "original" value of the destination of the read
106		@ allocate space on the stack to store the read: preserve alignment!
107		@ the passed default argument should be stored in case of EOF
108		sub sp, sp, #8
109		strb r0, [sp, #0]
110		mov r1, sp
111		adr r0, .L._readc_str0
112		bl scanf
113		ldrsb r0, [sp, #0]
114		add sp, sp, #8
115		mov sp, fp
116		pop {fp, pc}
117	
118	@ length of .L._prints_str0
119		.word 4
120	.L._prints_str0:
121		.asciz "%.*s"
122	.align 4
123	_prints:
124		push {fp, lr}
125		mov fp, sp
126		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
127		bic sp, sp, #0x7
128		mov r2, r0
129		ldr r1, [r0, #-4]
130		adr r0, .L._prints_str0
131		bl printf
132		mov r0, #0
133		bl fflush
134		mov sp, fp
135		pop {fp, pc}
136	
137	@ length of .L._readi_str0
138		.word 2
139	.L._readi_str0:
140		.asciz "%d"
141	.align 4
142	_readi:
143		push {fp, lr}
144		mov fp, sp
145		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
146		bic sp, sp, #0x7
147		@ R0 contains the "original" value of the destination of the read
148		@ allocate space on the stack to store the read: preserve alignment!
149		@ the passed default argument should be stored in case of EOF
150		sub sp, sp, #8
151		str r0, [sp, #0]
152		mov r1, sp
153		adr r0, .L._readi_str0
154		bl scanf
155		ldr r0, [sp, #0]
156		add sp, sp, #8
157		mov sp, fp
158		pop {fp, pc}
159	
160	@ length of .L._printi_str0
161		.word 2
162	.L._printi_str0:
163		.asciz "%d"
164	.align 4
165	_printi:
166		push {fp, lr}
167		mov fp, sp
168		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
169		bic sp, sp, #0x7
170		mov r1, r0
171		adr r0, .L._printi_str0
172		bl printf
173		mov r0, #0
174		bl fflush
175		mov sp, fp
176		pop {fp, pc}
177	
178	@ length of .L._println_str0
179		.word 0
180	.L._println_str0:
181		.asciz ""
182	.align 4
183	_println:
184		push {fp, lr}
185		mov fp, sp
186		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
187		bic sp, sp, #0x7
188		adr r0, .L._println_str0
189		bl puts
190		mov r0, #0
191		bl fflush
192		mov sp, fp
193		pop {fp, pc}
===========================================================
-- Finished

