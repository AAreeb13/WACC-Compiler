./valid/scope/ifNested2.wacc
calling the reference compiler on ./valid/scope/ifNested2.wacc
-- Test: ifNested2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Deeply nested conditional statement

# Output:
# correct
#

# Program:

begin
  int a = 13;
  if a == 13
  then
    if a > 5
    then
      if a < 10
      then
        println "incorrect"
      else
        if a > 12
        then
          if a > 13
          then
            println "incorrect"
          else
            println "correct"
          fi
        else
          println "incorrect"
        fi
      fi
    else
      println "incorrect"
    fi
  else
    println "incorrect"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ifNested2.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 9
3	.L.str0:
4		.asciz "incorrect"
5	// length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "correct"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #13
20		mov x19, x8
21		cmp x19, #13
22		b.eq .L0
23		// Stack pointer unchanged, no stack allocated arguments
24		adrp x8, .L.str0
25		add x8, x8, :lo12:.L.str0
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _prints
34		bl _println
35		b .L1
36	.L0:
37		cmp x19, #5
38		b.gt .L2
39		// Stack pointer unchanged, no stack allocated arguments
40		adrp x8, .L.str0
41		add x8, x8, :lo12:.L.str0
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		bl _println
51		b .L3
52	.L2:
53		cmp x19, #10
54		b.lt .L4
55		cmp x19, #12
56		b.gt .L6
57		// Stack pointer unchanged, no stack allocated arguments
58		adrp x8, .L.str0
59		add x8, x8, :lo12:.L.str0
60		// push {x8}
61		stp x8, xzr, [sp, #-16]!
62		// pop {x8}
63		ldp x8, xzr, [sp], #16
64		mov x8, x8
65		mov x0, x8
66		// statement primitives do not return results (but will clobber r0/rax)
67		bl _prints
68		bl _println
69		b .L7
70	.L6:
71		cmp x19, #13
72		b.gt .L8
73		// Stack pointer unchanged, no stack allocated arguments
74		adrp x8, .L.str1
75		add x8, x8, :lo12:.L.str1
76		// push {x8}
77		stp x8, xzr, [sp, #-16]!
78		// pop {x8}
79		ldp x8, xzr, [sp], #16
80		mov x8, x8
81		mov x0, x8
82		// statement primitives do not return results (but will clobber r0/rax)
83		bl _prints
84		bl _println
85		b .L9
86	.L8:
87		// Stack pointer unchanged, no stack allocated arguments
88		adrp x8, .L.str0
89		add x8, x8, :lo12:.L.str0
90		// push {x8}
91		stp x8, xzr, [sp, #-16]!
92		// pop {x8}
93		ldp x8, xzr, [sp], #16
94		mov x8, x8
95		mov x0, x8
96		// statement primitives do not return results (but will clobber r0/rax)
97		bl _prints
98		bl _println
99	.L9:
100	.L7:
101		b .L5
102	.L4:
103		// Stack pointer unchanged, no stack allocated arguments
104		adrp x8, .L.str0
105		add x8, x8, :lo12:.L.str0
106		// push {x8}
107		stp x8, xzr, [sp, #-16]!
108		// pop {x8}
109		ldp x8, xzr, [sp], #16
110		mov x8, x8
111		mov x0, x8
112		// statement primitives do not return results (but will clobber r0/rax)
113		bl _prints
114		bl _println
115	.L5:
116	.L3:
117	.L1:
118		// Stack pointer unchanged, no stack allocated variables
119		mov x0, #0
120		// pop {x19}
121		ldp x19, xzr, [sp], #16
122		// pop {fp, lr}
123		ldp fp, lr, [sp], #16
124		ret
125	
126	// length of .L._prints_str0
127		.word 4
128	.L._prints_str0:
129		.asciz "%.*s"
130	.align 4
131	_prints:
132		// push {lr}
133		stp lr, xzr, [sp, #-16]!
134		mov x2, x0
135		ldrsw x1, [x0, #-4]
136		adr x0, .L._prints_str0
137		bl printf
138		mov x0, #0
139		bl fflush
140		// pop {lr}
141		ldp lr, xzr, [sp], #16
142		ret
143	
144	// length of .L._println_str0
145		.word 0
146	.L._println_str0:
147		.asciz ""
148	.align 4
149	_println:
150		// push {lr}
151		stp lr, xzr, [sp, #-16]!
152		adr x0, .L._println_str0
153		bl puts
154		mov x0, #0
155		bl fflush
156		// pop {lr}
157		ldp lr, xzr, [sp], #16
158		ret
===========================================================
-- Finished

