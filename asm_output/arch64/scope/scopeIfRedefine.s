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
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #12
12		mov x19, x8
13		cmp x19, #12
14		b.eq .L0
15		// Stack pointer unchanged, no stack allocated variables
16		mov x8, #97
17		mov x20, x8
18		// Stack pointer unchanged, no stack allocated arguments
19		mov x8, x20
20		mov x0, x8
21		// statement primitives do not return results (but will clobber r0/rax)
22		bl _printc
23		bl _println
24		// Stack pointer unchanged, no stack allocated variables
25		b .L1
26	.L0:
27		// Stack pointer unchanged, no stack allocated variables
28		mov x8, #1
29		mov x20, x8
30		// Stack pointer unchanged, no stack allocated arguments
31		mov x8, x20
32		mov x0, x8
33		// statement primitives do not return results (but will clobber r0/rax)
34		bl _printb
35		bl _println
36		// Stack pointer unchanged, no stack allocated variables
37	.L1:
38		// Stack pointer unchanged, no stack allocated arguments
39		mov x8, x19
40		mov x0, x8
41		// statement primitives do not return results (but will clobber r0/rax)
42		bl _printi
43		bl _println
44		// Stack pointer unchanged, no stack allocated variables
45		mov x0, #0
46		// pop {x19, x20}
47		ldp x19, x20, [sp], #16
48		// pop {fp, lr}
49		ldp fp, lr, [sp], #16
50		ret
51	
52	// length of .L._printi_str0
53		.word 2
54	.L._printi_str0:
55		.asciz "%d"
56	.align 4
57	_printi:
58		// push {lr}
59		stp lr, xzr, [sp, #-16]!
60		mov x1, x0
61		adr x0, .L._printi_str0
62		bl printf
63		mov x0, #0
64		bl fflush
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
68	
69	// length of .L._printb_str0
70		.word 5
71	.L._printb_str0:
72		.asciz "false"
73	// length of .L._printb_str1
74		.word 4
75	.L._printb_str1:
76		.asciz "true"
77	// length of .L._printb_str2
78		.word 4
79	.L._printb_str2:
80		.asciz "%.*s"
81	.align 4
82	_printb:
83		// push {lr}
84		stp lr, xzr, [sp, #-16]!
85		cmp w0, #0
86		b.ne .L_printb0
87		adr x2, .L._printb_str0
88		b .L_printb1
89	.L_printb0:
90		adr x2, .L._printb_str1
91	.L_printb1:
92		ldrsw x1, [x2, #-4]
93		adr x0, .L._printb_str2
94		bl printf
95		mov x0, #0
96		bl fflush
97		// pop {lr}
98		ldp lr, xzr, [sp], #16
99		ret
100	
101	// length of .L._printc_str0
102		.word 2
103	.L._printc_str0:
104		.asciz "%c"
105	.align 4
106	_printc:
107		// push {lr}
108		stp lr, xzr, [sp, #-16]!
109		mov x1, x0
110		adr x0, .L._printc_str0
111		bl printf
112		mov x0, #0
113		bl fflush
114		// pop {lr}
115		ldp lr, xzr, [sp], #16
116		ret
117	
118	// length of .L._println_str0
119		.word 0
120	.L._println_str0:
121		.asciz ""
122	.align 4
123	_println:
124		// push {lr}
125		stp lr, xzr, [sp, #-16]!
126		adr x0, .L._println_str0
127		bl puts
128		mov x0, #0
129		bl fflush
130		// pop {lr}
131		ldp lr, xzr, [sp], #16
132		ret
===========================================================
-- Finished

