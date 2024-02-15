./valid/function/nested_functions/printTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printTriangle.wacc
-- Test: printTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a fixed size triangle

# Output:
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

  int s = call f(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printTriangle.s contents are:
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
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		@ Stack pointer unchanged, no stack allocated arguments
18		mov r8, #8
19		mov r0, r8
20		bl wacc_f
21		mov r12, r0
22		@ Stack pointer unchanged, no stack allocated arguments
23		mov r8, r12
24		mov r4, r8
25		@ Stack pointer unchanged, no stack allocated variables
26		mov r0, #0
27		pop {r4, r8, r10, r12}
28		pop {fp, pc}
29	
30	wacc_f:
31		push {fp, lr}
32		push {r4, r5}
33		mov fp, sp
34		cmp r0, #0
35		beq .L0
36		@ Stack pointer unchanged, no stack allocated variables
37		mov r8, r0
38		mov r4, r8
39		b .L2
40	.L3:
41		push {r0}
42		@ Set up R12 as a temporary second base pointer for the caller saved things
43		mov r12, sp
44		@ Stack pointer unchanged, no stack allocated arguments
45		ldr r8, =.L.str0
46		push {r8}
47		pop {r8}
48		mov r8, r8
49		mov r0, r8
50		@ statement primitives do not return results (but will clobber r0/rax)
51		bl _prints
52		pop {r0}
53		subs r8, r4, #1
54		blvs _errOverflow
55		push {r8}
56		pop {r8}
57		mov r8, r8
58		mov r4, r8
59	.L2:
60		cmp r4, #0
61		bgt .L3
62		push {r0}
63		@ Set up R12 as a temporary second base pointer for the caller saved things
64		mov r12, sp
65		@ Stack pointer unchanged, no stack allocated arguments
66		ldr r8, =.L.str1
67		push {r8}
68		pop {r8}
69		mov r8, r8
70		mov r0, r8
71		@ statement primitives do not return results (but will clobber r0/rax)
72		bl _prints
73		bl _println
74		pop {r0}
75		push {r0}
76		@ Set up R12 as a temporary second base pointer for the caller saved things
77		mov r12, sp
78		@ Stack pointer unchanged, no stack allocated arguments
79		subs r8, r0, #1
80		blvs _errOverflow
81		push {r8}
82		pop {r8}
83		mov r8, r8
84		mov r0, r8
85		bl wacc_f
86		mov r12, r0
87		@ Stack pointer unchanged, no stack allocated arguments
88		pop {r0}
89		mov r8, r12
90		mov r5, r8
91		@ Stack pointer unchanged, no stack allocated variables
92		b .L1
93	.L0:
94	.L1:
95		mov r0, #0
96		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
97		mov sp, fp
98		pop {r4, r5}
99		pop {fp, pc}
100		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
101		@ the .ltorg here is used to generate the constant pool after every function
102		@ this is important for ensuring string literals and large integers are accessible!
103		.ltorg
104	
105	@ length of .L._prints_str0
106		.word 4
107	.L._prints_str0:
108		.asciz "%.*s"
109	.align 4
110	_prints:
111		push {fp, lr}
112		mov fp, sp
113		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
114		bic sp, sp, #0x7
115		mov r2, r0
116		ldr r1, [r0, #-4]
117		adr r0, .L._prints_str0
118		bl printf
119		mov r0, #0
120		bl fflush
121		mov sp, fp
122		pop {fp, pc}
123	
124	@ length of .L._println_str0
125		.word 0
126	.L._println_str0:
127		.asciz ""
128	.align 4
129	_println:
130		push {fp, lr}
131		mov fp, sp
132		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
133		bic sp, sp, #0x7
134		adr r0, .L._println_str0
135		bl puts
136		mov r0, #0
137		bl fflush
138		mov sp, fp
139		pop {fp, pc}
140	
141	_exit:
142		push {fp, lr}
143		mov fp, sp
144		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
145		bic sp, sp, #0x7
146		bl exit
147		mov sp, fp
148		pop {fp, pc}
149	
150	@ length of .L._errOverflow_str0
151		.word 52
152	.L._errOverflow_str0:
153		.asciz "fatal error: integer overflow or underflow occurred\n"
154	.align 4
155	_errOverflow:
156		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
157		bic sp, sp, #0x7
158		adr r0, .L._errOverflow_str0
159		bl _prints
160		mov r0, #255
161		bl _exit
===========================================================
-- Finished

