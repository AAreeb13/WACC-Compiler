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
1	@ length of .L.str0
2		.word 9
3	.L.str0:
4		.asciz "incorrect"
5	@ length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "correct"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		mov r8, #13
18		mov r4, r8
19		cmp r4, #13
20		beq .L0
21		@ Stack pointer unchanged, no stack allocated arguments
22		ldr r8, =.L.str0
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r0, r8
27		@ statement primitives do not return results (but will clobber r0/rax)
28		bl _prints
29		bl _println
30		b .L1
31	.L0:
32		cmp r4, #5
33		bgt .L2
34		@ Stack pointer unchanged, no stack allocated arguments
35		ldr r8, =.L.str0
36		push {r8}
37		pop {r8}
38		mov r8, r8
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		bl _println
43		b .L3
44	.L2:
45		cmp r4, #10
46		blt .L4
47		cmp r4, #12
48		bgt .L6
49		@ Stack pointer unchanged, no stack allocated arguments
50		ldr r8, =.L.str0
51		push {r8}
52		pop {r8}
53		mov r8, r8
54		mov r0, r8
55		@ statement primitives do not return results (but will clobber r0/rax)
56		bl _prints
57		bl _println
58		b .L7
59	.L6:
60		cmp r4, #13
61		bgt .L8
62		@ Stack pointer unchanged, no stack allocated arguments
63		ldr r8, =.L.str1
64		push {r8}
65		pop {r8}
66		mov r8, r8
67		mov r0, r8
68		@ statement primitives do not return results (but will clobber r0/rax)
69		bl _prints
70		bl _println
71		b .L9
72	.L8:
73		@ Stack pointer unchanged, no stack allocated arguments
74		ldr r8, =.L.str0
75		push {r8}
76		pop {r8}
77		mov r8, r8
78		mov r0, r8
79		@ statement primitives do not return results (but will clobber r0/rax)
80		bl _prints
81		bl _println
82	.L9:
83	.L7:
84		b .L5
85	.L4:
86		@ Stack pointer unchanged, no stack allocated arguments
87		ldr r8, =.L.str0
88		push {r8}
89		pop {r8}
90		mov r8, r8
91		mov r0, r8
92		@ statement primitives do not return results (but will clobber r0/rax)
93		bl _prints
94		bl _println
95	.L5:
96	.L3:
97	.L1:
98		@ Stack pointer unchanged, no stack allocated variables
99		mov r0, #0
100		pop {r4, r8, r10, r12}
101		pop {fp, pc}
102	
103	@ length of .L._prints_str0
104		.word 4
105	.L._prints_str0:
106		.asciz "%.*s"
107	.align 4
108	_prints:
109		push {fp, lr}
110		mov fp, sp
111		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
112		bic sp, sp, #0x7
113		mov r2, r0
114		ldr r1, [r0, #-4]
115		adr r0, .L._prints_str0
116		bl printf
117		mov r0, #0
118		bl fflush
119		mov sp, fp
120		pop {fp, pc}
121	
122	@ length of .L._println_str0
123		.word 0
124	.L._println_str0:
125		.asciz ""
126	.align 4
127	_println:
128		push {fp, lr}
129		mov fp, sp
130		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
131		bic sp, sp, #0x7
132		adr r0, .L._println_str0
133		bl puts
134		mov r0, #0
135		bl fflush
136		mov sp, fp
137		pop {fp, pc}
===========================================================
-- Finished

