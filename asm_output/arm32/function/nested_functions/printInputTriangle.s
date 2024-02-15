./valid/function/nested_functions/printInputTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printInputTriangle.wacc
-- Test: printInputTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a user-specified sized triangle

# Input: 13

# Output:
# Please enter the size of the triangle to print:
# -------------
# ------------
# -----------
# ----------
# ---------
# --------
# -------
# ------
# -----
# ----
# ---
# --
# -
#

# Program:

begin
  int f(int x) is
    if x == 0 then
      skip
    else
      int i = x ;
      while i > 0 do
        print "-" ;
        i = i - 1
      done ;
      println "" ;
      int s = call f(x - 1)
    fi ;
    return 0
  end

  println "Please enter the size of the triangle to print:" ;
  int x = 0;

  read x;
  int s = call f(x)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printInputTriangle.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 1
3	.L.str0:
4		.asciz "-"
5	@ length of .L.str1
6		.word 0
7	.L.str1:
8		.asciz ""
9	@ length of .L.str2
10		.word 47
11	.L.str2:
12		.asciz "Please enter the size of the triangle to print:"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		@ Stack pointer unchanged, no stack allocated arguments
22		ldr r8, =.L.str2
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _prints
29		bl _println
30		mov r8, #0
31		mov r4, r8
32		@ Stack pointer unchanged, no stack allocated arguments
33		@ load the current value in the destination of the read so it supports defaults
34		mov r8, r4
35		mov r0, r8
36		bl _readi
37		mov r12, r0
38		mov r8, r12
39		mov r4, r8
40		@ Stack pointer unchanged, no stack allocated arguments
41		mov r8, r4
42		mov r0, r8
43		bl wacc_f
44		mov r12, r0
45		@ Stack pointer unchanged, no stack allocated arguments
46		mov r8, r12
47		mov r5, r8
48		@ Stack pointer unchanged, no stack allocated variables
49		mov r0, #0
50		pop {r4, r5, r8, r10, r12}
51		pop {fp, pc}
52	
53	wacc_f:
54		push {fp, lr}
55		push {r4, r5}
56		mov fp, sp
57		cmp r0, #0
58		beq .L0
59		@ Stack pointer unchanged, no stack allocated variables
60		mov r8, r0
61		mov r4, r8
62		b .L2
63	.L3:
64		push {r0}
65		@ Set up R12 as a temporary second base pointer for the caller saved things
66		mov r12, sp
67		@ Stack pointer unchanged, no stack allocated arguments
68		ldr r8, =.L.str0
69		push {r8}
70		pop {r8}
71		mov r8, r8
72		mov r0, r8
73		@ statement primitives do not return results (but will clobber r0/rax)
74		bl _prints
75		pop {r0}
76		subs r8, r4, #1
77		blvs _errOverflow
78		push {r8}
79		pop {r8}
80		mov r8, r8
81		mov r4, r8
82	.L2:
83		cmp r4, #0
84		bgt .L3
85		push {r0}
86		@ Set up R12 as a temporary second base pointer for the caller saved things
87		mov r12, sp
88		@ Stack pointer unchanged, no stack allocated arguments
89		ldr r8, =.L.str1
90		push {r8}
91		pop {r8}
92		mov r8, r8
93		mov r0, r8
94		@ statement primitives do not return results (but will clobber r0/rax)
95		bl _prints
96		bl _println
97		pop {r0}
98		push {r0}
99		@ Set up R12 as a temporary second base pointer for the caller saved things
100		mov r12, sp
101		@ Stack pointer unchanged, no stack allocated arguments
102		subs r8, r0, #1
103		blvs _errOverflow
104		push {r8}
105		pop {r8}
106		mov r8, r8
107		mov r0, r8
108		bl wacc_f
109		mov r12, r0
110		@ Stack pointer unchanged, no stack allocated arguments
111		pop {r0}
112		mov r8, r12
113		mov r5, r8
114		@ Stack pointer unchanged, no stack allocated variables
115		b .L1
116	.L0:
117	.L1:
118		mov r0, #0
119		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
120		mov sp, fp
121		pop {r4, r5}
122		pop {fp, pc}
123		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
124		@ the .ltorg here is used to generate the constant pool after every function
125		@ this is important for ensuring string literals and large integers are accessible!
126		.ltorg
127	
128	@ length of .L._prints_str0
129		.word 4
130	.L._prints_str0:
131		.asciz "%.*s"
132	.align 4
133	_prints:
134		push {fp, lr}
135		mov fp, sp
136		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
137		bic sp, sp, #0x7
138		mov r2, r0
139		ldr r1, [r0, #-4]
140		adr r0, .L._prints_str0
141		bl printf
142		mov r0, #0
143		bl fflush
144		mov sp, fp
145		pop {fp, pc}
146	
147	@ length of .L._readi_str0
148		.word 2
149	.L._readi_str0:
150		.asciz "%d"
151	.align 4
152	_readi:
153		push {fp, lr}
154		mov fp, sp
155		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
156		bic sp, sp, #0x7
157		@ R0 contains the "original" value of the destination of the read
158		@ allocate space on the stack to store the read: preserve alignment!
159		@ the passed default argument should be stored in case of EOF
160		sub sp, sp, #8
161		str r0, [sp, #0]
162		mov r1, sp
163		adr r0, .L._readi_str0
164		bl scanf
165		ldr r0, [sp, #0]
166		add sp, sp, #8
167		mov sp, fp
168		pop {fp, pc}
169	
170	@ length of .L._println_str0
171		.word 0
172	.L._println_str0:
173		.asciz ""
174	.align 4
175	_println:
176		push {fp, lr}
177		mov fp, sp
178		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
179		bic sp, sp, #0x7
180		adr r0, .L._println_str0
181		bl puts
182		mov r0, #0
183		bl fflush
184		mov sp, fp
185		pop {fp, pc}
186	
187	_exit:
188		push {fp, lr}
189		mov fp, sp
190		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
191		bic sp, sp, #0x7
192		bl exit
193		mov sp, fp
194		pop {fp, pc}
195	
196	@ length of .L._errOverflow_str0
197		.word 52
198	.L._errOverflow_str0:
199		.asciz "fatal error: integer overflow or underflow occurred\n"
200	.align 4
201	_errOverflow:
202		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
203		bic sp, sp, #0x7
204		adr r0, .L._errOverflow_str0
205		bl _prints
206		mov r0, #255
207		bl _exit
===========================================================
-- Finished

