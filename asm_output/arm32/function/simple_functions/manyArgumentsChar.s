./valid/function/simple_functions/manyArgumentsChar.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsChar.wacc
-- Test: manyArgumentsChar.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# A
# b
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  char f(int s, int t, int u, int v, int w, int x, char y, bool z) is
    int i = u + v ;
    int j = w + x ;
    if z
        then return chr (ord y - i * j)
        else return y
    fi
  end

  char r1 = call f(0, 0, 3, 5, 1, 3, 'a', true) ;
  println r1 ;

  char r2 = call f(0, 0, 3, 5, 1, 3, 'b', false) ;
  println r2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsChar.s contents are:
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
9		@ Stack padded to a multiple of the required alignment
10		sub sp, sp, #12
11		mov r8, #0
12		mov r0, r8
13		mov r8, #0
14		mov r1, r8
15		mov r8, #3
16		mov r2, r8
17		mov r8, #5
18		mov r3, r8
19		mov r8, #1
20		str r8, [sp, #6]
21		mov r8, #3
22		str r8, [sp, #2]
23		mov r8, #97
24		strb r8, [sp, #1]
25		mov r8, #1
26		strb r8, [sp, #0]
27		bl wacc_f
28		mov r12, r0
29		@ Stack padded to a multiple of the required alignment
30		add sp, sp, #12
31		mov r8, r12
32		mov r4, r8
33		@ Stack pointer unchanged, no stack allocated arguments
34		mov r8, r4
35		mov r0, r8
36		@ statement primitives do not return results (but will clobber r0/rax)
37		bl _printc
38		bl _println
39		@ Stack padded to a multiple of the required alignment
40		sub sp, sp, #12
41		mov r8, #0
42		mov r0, r8
43		mov r8, #0
44		mov r1, r8
45		mov r8, #3
46		mov r2, r8
47		mov r8, #5
48		mov r3, r8
49		mov r8, #1
50		str r8, [sp, #6]
51		mov r8, #3
52		str r8, [sp, #2]
53		mov r8, #98
54		strb r8, [sp, #1]
55		mov r8, #0
56		strb r8, [sp, #0]
57		bl wacc_f
58		mov r12, r0
59		@ Stack padded to a multiple of the required alignment
60		add sp, sp, #12
61		mov r8, r12
62		mov r5, r8
63		@ Stack pointer unchanged, no stack allocated arguments
64		mov r8, r5
65		mov r0, r8
66		@ statement primitives do not return results (but will clobber r0/rax)
67		bl _printc
68		bl _println
69		@ Stack pointer unchanged, no stack allocated variables
70		mov r0, #0
71		pop {r4, r5, r8, r10, r12}
72		pop {fp, pc}
73	
74	wacc_f:
75		push {fp, lr}
76		push {r4, r5}
77		mov fp, sp
78		@ Stack pointer unchanged, no stack allocated variables
79		adds r8, r2, r3
80		blvs _errOverflow
81		push {r8}
82		pop {r8}
83		mov r8, r8
84		mov r4, r8
85		ldr r9, [fp, #18]
86		ldr r8, [fp, #22]
87		adds r8, r8, r9
88		blvs _errOverflow
89		push {r8}
90		pop {r8}
91		mov r8, r8
92		mov r5, r8
93		ldrsb r8, [fp, #16]
94		cmp r8, #1
95		beq .L0
96		ldrsb r0, [fp, #17]
97		mov r0, r0
98		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
99		mov sp, fp
100		pop {r4, r5}
101		pop {fp, pc}
102		b .L1
103	.L0:
104		smull r8, r9, r4, r5
105		@ Compare the sign of the lowest bits of the multiplication against the highest
106		cmp r9, r8, asr #31
107		blne _errOverflow
108		push {r8}
109		pop {r9}
110		ldrsb r8, [fp, #17]
111		subs r8, r8, r9
112		blvs _errOverflow
113		push {r8}
114		pop {r8}
115		mov r8, r8
116		ldr r9, =-128
117		tst r8, r9
118		movne r1, r8
119		blne _errBadChar
120		push {r8}
121		pop {r0}
122		mov r0, r0
123		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
124		mov sp, fp
125		pop {r4, r5}
126		pop {fp, pc}
127	.L1:
128		@ Stack pointer unchanged, no stack allocated variables
129		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
130		@ the .ltorg here is used to generate the constant pool after every function
131		@ this is important for ensuring string literals and large integers are accessible!
132		.ltorg
133	
134	@ length of .L._prints_str0
135		.word 4
136	.L._prints_str0:
137		.asciz "%.*s"
138	.align 4
139	_prints:
140		push {fp, lr}
141		mov fp, sp
142		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
143		bic sp, sp, #0x7
144		mov r2, r0
145		ldr r1, [r0, #-4]
146		adr r0, .L._prints_str0
147		bl printf
148		mov r0, #0
149		bl fflush
150		mov sp, fp
151		pop {fp, pc}
152	
153	@ length of .L._printc_str0
154		.word 2
155	.L._printc_str0:
156		.asciz "%c"
157	.align 4
158	_printc:
159		push {fp, lr}
160		mov fp, sp
161		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
162		bic sp, sp, #0x7
163		mov r1, r0
164		adr r0, .L._printc_str0
165		bl printf
166		mov r0, #0
167		bl fflush
168		mov sp, fp
169		pop {fp, pc}
170	
171	@ length of .L._println_str0
172		.word 0
173	.L._println_str0:
174		.asciz ""
175	.align 4
176	_println:
177		push {fp, lr}
178		mov fp, sp
179		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
180		bic sp, sp, #0x7
181		adr r0, .L._println_str0
182		bl puts
183		mov r0, #0
184		bl fflush
185		mov sp, fp
186		pop {fp, pc}
187	
188	_exit:
189		push {fp, lr}
190		mov fp, sp
191		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
192		bic sp, sp, #0x7
193		bl exit
194		mov sp, fp
195		pop {fp, pc}
196	
197	@ length of .L._errBadChar_str0
198		.word 50
199	.L._errBadChar_str0:
200		.asciz "fatal error: int %d is not ascii character 0-127 \n"
201	.align 4
202	_errBadChar:
203		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
204		bic sp, sp, #0x7
205		adr r0, .L._errBadChar_str0
206		bl printf
207		mov r0, #0
208		bl fflush
209		mov r0, #255
210		bl _exit
211	
212	@ length of .L._errOverflow_str0
213		.word 52
214	.L._errOverflow_str0:
215		.asciz "fatal error: integer overflow or underflow occurred\n"
216	.align 4
217	_errOverflow:
218		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
219		bic sp, sp, #0x7
220		adr r0, .L._errOverflow_str0
221		bl _prints
222		mov r0, #255
223		bl _exit
===========================================================
-- Finished

