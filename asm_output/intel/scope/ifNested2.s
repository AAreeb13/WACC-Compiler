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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 9
5	.L.str0:
6		.asciz "incorrect"
7	# length of .L.str1
8		.int 7
9	.L.str1:
10		.asciz "correct"
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 13
21		mov r12, rax
22		cmp r12, 13
23		je .L0
24		# Stack pointer unchanged, no stack allocated arguments
25		lea rax, [rip + .L.str0]
26		push rax
27		pop rax
28		mov rax, rax
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _prints
32		call _println
33		jmp .L1
34	.L0:
35		cmp r12, 5
36		jg .L2
37		# Stack pointer unchanged, no stack allocated arguments
38		lea rax, [rip + .L.str0]
39		push rax
40		pop rax
41		mov rax, rax
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _prints
45		call _println
46		jmp .L3
47	.L2:
48		cmp r12, 10
49		jl .L4
50		cmp r12, 12
51		jg .L6
52		# Stack pointer unchanged, no stack allocated arguments
53		lea rax, [rip + .L.str0]
54		push rax
55		pop rax
56		mov rax, rax
57		mov rdi, rax
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _prints
60		call _println
61		jmp .L7
62	.L6:
63		cmp r12, 13
64		jg .L8
65		# Stack pointer unchanged, no stack allocated arguments
66		lea rax, [rip + .L.str1]
67		push rax
68		pop rax
69		mov rax, rax
70		mov rdi, rax
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _prints
73		call _println
74		jmp .L9
75	.L8:
76		# Stack pointer unchanged, no stack allocated arguments
77		lea rax, [rip + .L.str0]
78		push rax
79		pop rax
80		mov rax, rax
81		mov rdi, rax
82		# statement primitives do not return results (but will clobber r0/rax)
83		call _prints
84		call _println
85	.L9:
86	.L7:
87		jmp .L5
88	.L4:
89		# Stack pointer unchanged, no stack allocated arguments
90		lea rax, [rip + .L.str0]
91		push rax
92		pop rax
93		mov rax, rax
94		mov rdi, rax
95		# statement primitives do not return results (but will clobber r0/rax)
96		call _prints
97		call _println
98	.L5:
99	.L3:
100	.L1:
101		# Stack pointer unchanged, no stack allocated variables
102		mov rax, 0
103		# pop {rbx, r12}
104		mov rbx, qword ptr [rsp]
105		mov r12, qword ptr [rsp + 8]
106		add rsp, 16
107		pop rbp
108		ret
109	
110	.section .rodata
111	# length of .L._prints_str0
112		.int 4
113	.L._prints_str0:
114		.asciz "%.*s"
115	.text
116	_prints:
117		push rbp
118		mov rbp, rsp
119		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
120		and rsp, -16
121		mov rdx, rdi
122		mov esi, dword ptr [rdi - 4]
123		lea rdi, [rip + .L._prints_str0]
124		# on x86, al represents the number of SIMD registers used as variadic arguments
125		mov al, 0
126		call printf@plt
127		mov rdi, 0
128		call fflush@plt
129		mov rsp, rbp
130		pop rbp
131		ret
132	
133	.section .rodata
134	# length of .L._println_str0
135		.int 0
136	.L._println_str0:
137		.asciz ""
138	.text
139	_println:
140		push rbp
141		mov rbp, rsp
142		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
143		and rsp, -16
144		lea rdi, [rip + .L._println_str0]
145		call puts@plt
146		mov rdi, 0
147		call fflush@plt
148		mov rsp, rbp
149		pop rbp
150		ret
===========================================================
-- Finished

