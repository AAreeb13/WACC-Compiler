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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 9
4	.L.str0:
5		.asciz "incorrect"
6	# length of .L.str1
7		.int 7
8	.L.str1:
9		.asciz "correct"
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		movq $13, %rax
20		movq %rax, %r12
21		cmpq $13, %r12
22		je .L0
23		# Stack pointer unchanged, no stack allocated arguments
24		leaq .L.str0(%rip), %rax
25		pushq %rax
26		popq %rax
27		movq %rax, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _prints
31		call _println
32		jmp .L1
33	.L0:
34		cmpq $5, %r12
35		jg .L2
36		# Stack pointer unchanged, no stack allocated arguments
37		leaq .L.str0(%rip), %rax
38		pushq %rax
39		popq %rax
40		movq %rax, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _prints
44		call _println
45		jmp .L3
46	.L2:
47		cmpq $10, %r12
48		jl .L4
49		cmpq $12, %r12
50		jg .L6
51		# Stack pointer unchanged, no stack allocated arguments
52		leaq .L.str0(%rip), %rax
53		pushq %rax
54		popq %rax
55		movq %rax, %rax
56		movq %rax, %rdi
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _prints
59		call _println
60		jmp .L7
61	.L6:
62		cmpq $13, %r12
63		jg .L8
64		# Stack pointer unchanged, no stack allocated arguments
65		leaq .L.str1(%rip), %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %rdi
70		# statement primitives do not return results (but will clobber r0/rax)
71		call _prints
72		call _println
73		jmp .L9
74	.L8:
75		# Stack pointer unchanged, no stack allocated arguments
76		leaq .L.str0(%rip), %rax
77		pushq %rax
78		popq %rax
79		movq %rax, %rax
80		movq %rax, %rdi
81		# statement primitives do not return results (but will clobber r0/rax)
82		call _prints
83		call _println
84	.L9:
85	.L7:
86		jmp .L5
87	.L4:
88		# Stack pointer unchanged, no stack allocated arguments
89		leaq .L.str0(%rip), %rax
90		pushq %rax
91		popq %rax
92		movq %rax, %rax
93		movq %rax, %rdi
94		# statement primitives do not return results (but will clobber r0/rax)
95		call _prints
96		call _println
97	.L5:
98	.L3:
99	.L1:
100		# Stack pointer unchanged, no stack allocated variables
101		movq $0, %rax
102		# popq {%rbx, %r12}
103		movq (%rsp), %rbx
104		movq 8(%rsp), %r12
105		addq $16, %rsp
106		popq %rbp
107		ret
108	
109	.section .rodata
110	# length of .L._prints_str0
111		.int 4
112	.L._prints_str0:
113		.asciz "%.*s"
114	.text
115	_prints:
116		pushq %rbp
117		movq %rsp, %rbp
118		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
119		andq $-16, %rsp
120		movq %rdi, %rdx
121		movl -4(%rdi), %esi
122		leaq .L._prints_str0(%rip), %rdi
123		# on x86, al represents the number of SIMD registers used as variadic arguments
124		movb $0, %al
125		call printf@plt
126		movq $0, %rdi
127		call fflush@plt
128		movq %rbp, %rsp
129		popq %rbp
130		ret
131	
132	.section .rodata
133	# length of .L._println_str0
134		.int 0
135	.L._println_str0:
136		.asciz ""
137	.text
138	_println:
139		pushq %rbp
140		movq %rsp, %rbp
141		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
142		andq $-16, %rsp
143		leaq .L._println_str0(%rip), %rdi
144		call puts@plt
145		movq $0, %rdi
146		call fflush@plt
147		movq %rbp, %rsp
148		popq %rbp
149		ret
===========================================================
-- Finished

