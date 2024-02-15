./valid/function/nested_functions/mutualRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/mutualRecursion.wacc
-- Test: mutualRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a pair of mutually recursive functions

# Output:
# r1: sending 8
# r2: received 8
# r1: sending 7
# r2: received 7
# r1: sending 6
# r2: received 6
# r1: sending 5
# r2: received 5
# r1: sending 4
# r2: received 4
# r1: sending 3
# r2: received 3
# r1: sending 2
# r2: received 2
# r1: sending 1
# r2: received 1
#

# Program:

begin
  int r1(int x) is
    if x == 0
    then
      skip
    else
      print "r1: sending " ;
      println x ;
      int y = call r2(x)
    fi ;
    return 42
  end

  int r2(int y) is
    print "r2: received " ;
    println y ;
    int z = call r1(y - 1) ;
    return 44
  end

  int x = 0 ;
  x = call r1(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
mutualRecursion.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "r1: sending "
5	@ length of .L.str1
6		.word 13
7	.L.str1:
8		.asciz "r2: received "
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #0
18		mov r4, r8
19		@ Stack pointer unchanged, no stack allocated arguments
20		mov r8, #8
21		mov r0, r8
22		bl wacc_r1
23		mov r12, r0
24		@ Stack pointer unchanged, no stack allocated arguments
25		mov r8, r12
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated variables
28		mov r0, #0
29		pop {r4, r8, r10, r12}
30		pop {fp, pc}
31	
32	wacc_r1:
33		push {fp, lr}
34		push {r4}
35		mov fp, sp
36		cmp r0, #0
37		beq .L0
38		@ Stack pointer unchanged, no stack allocated variables
39		push {r0}
40		@ Set up R12 as a temporary second base pointer for the caller saved things
41		mov r12, sp
42		@ Stack pointer unchanged, no stack allocated arguments
43		ldr r8, =.L.str0
44		push {r8}
45		pop {r8}
46		mov r8, r8
47		mov r0, r8
48		@ statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		pop {r0}
51		push {r0}
52		@ Set up R12 as a temporary second base pointer for the caller saved things
53		mov r12, sp
54		@ Stack pointer unchanged, no stack allocated arguments
55		mov r8, r0
56		mov r0, r8
57		@ statement primitives do not return results (but will clobber r0/rax)
58		bl _printi
59		bl _println
60		pop {r0}
61		push {r0}
62		@ Set up R12 as a temporary second base pointer for the caller saved things
63		mov r12, sp
64		@ Stack pointer unchanged, no stack allocated arguments
65		mov r8, r0
66		mov r0, r8
67		bl wacc_r2
68		mov r12, r0
69		@ Stack pointer unchanged, no stack allocated arguments
70		pop {r0}
71		mov r8, r12
72		mov r4, r8
73		@ Stack pointer unchanged, no stack allocated variables
74		b .L1
75	.L0:
76	.L1:
77		mov r0, #42
78		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
79		mov sp, fp
80		pop {r4}
81		pop {fp, pc}
82		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
83		@ the .ltorg here is used to generate the constant pool after every function
84		@ this is important for ensuring string literals and large integers are accessible!
85		.ltorg
86	
87	wacc_r2:
88		push {fp, lr}
89		push {r4}
90		mov fp, sp
91		@ Stack pointer unchanged, no stack allocated variables
92		push {r0}
93		@ Set up R12 as a temporary second base pointer for the caller saved things
94		mov r12, sp
95		@ Stack pointer unchanged, no stack allocated arguments
96		ldr r8, =.L.str1
97		push {r8}
98		pop {r8}
99		mov r8, r8
100		mov r0, r8
101		@ statement primitives do not return results (but will clobber r0/rax)
102		bl _prints
103		pop {r0}
104		push {r0}
105		@ Set up R12 as a temporary second base pointer for the caller saved things
106		mov r12, sp
107		@ Stack pointer unchanged, no stack allocated arguments
108		mov r8, r0
109		mov r0, r8
110		@ statement primitives do not return results (but will clobber r0/rax)
111		bl _printi
112		bl _println
113		pop {r0}
114		push {r0}
115		@ Set up R12 as a temporary second base pointer for the caller saved things
116		mov r12, sp
117		@ Stack pointer unchanged, no stack allocated arguments
118		subs r8, r0, #1
119		blvs _errOverflow
120		push {r8}
121		pop {r8}
122		mov r8, r8
123		mov r0, r8
124		bl wacc_r1
125		mov r12, r0
126		@ Stack pointer unchanged, no stack allocated arguments
127		pop {r0}
128		mov r8, r12
129		mov r4, r8
130		mov r0, #44
131		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
132		mov sp, fp
133		pop {r4}
134		pop {fp, pc}
135		@ Stack pointer unchanged, no stack allocated variables
136		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
137		@ the .ltorg here is used to generate the constant pool after every function
138		@ this is important for ensuring string literals and large integers are accessible!
139		.ltorg
140	
141	@ length of .L._prints_str0
142		.word 4
143	.L._prints_str0:
144		.asciz "%.*s"
145	.align 4
146	_prints:
147		push {fp, lr}
148		mov fp, sp
149		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
150		bic sp, sp, #0x7
151		mov r2, r0
152		ldr r1, [r0, #-4]
153		adr r0, .L._prints_str0
154		bl printf
155		mov r0, #0
156		bl fflush
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
194	
195	_exit:
196		push {fp, lr}
197		mov fp, sp
198		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
199		bic sp, sp, #0x7
200		bl exit
201		mov sp, fp
202		pop {fp, pc}
203	
204	@ length of .L._errOverflow_str0
205		.word 52
206	.L._errOverflow_str0:
207		.asciz "fatal error: integer overflow or underflow occurred\n"
208	.align 4
209	_errOverflow:
210		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
211		bic sp, sp, #0x7
212		adr r0, .L._errOverflow_str0
213		bl _prints
214		mov r0, #255
215		bl _exit
===========================================================
-- Finished

