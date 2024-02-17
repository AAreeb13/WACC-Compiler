./valid/function/simple_functions/asciiTable.wacc
calling the reference compiler on ./valid/function/simple_functions/asciiTable.wacc
-- Test: asciiTable.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print out a lookup table for Ascii character representations

# Output:
# Ascii character lookup table:
# -------------
# |   32 =    |
# |   33 = !  |
# |   34 = "  |
# |   35 = #  |
# |   36 = $  |
# |   37 = %  |
# |   38 = &  |
# |   39 = '  |
# |   40 = (  |
# |   41 = )  |
# |   42 = *  |
# |   43 = +  |
# |   44 = ,  |
# |   45 = -  |
# |   46 = .  |
# |   47 = /  |
# |   48 = 0  |
# |   49 = 1  |
# |   50 = 2  |
# |   51 = 3  |
# |   52 = 4  |
# |   53 = 5  |
# |   54 = 6  |
# |   55 = 7  |
# |   56 = 8  |
# |   57 = 9  |
# |   58 = :  |
# |   59 = ;  |
# |   60 = <  |
# |   61 = =  |
# |   62 = >  |
# |   63 = ?  |
# |   64 = @  |
# |   65 = A  |
# |   66 = B  |
# |   67 = C  |
# |   68 = D  |
# |   69 = E  |
# |   70 = F  |
# |   71 = G  |
# |   72 = H  |
# |   73 = I  |
# |   74 = J  |
# |   75 = K  |
# |   76 = L  |
# |   77 = M  |
# |   78 = N  |
# |   79 = O  |
# |   80 = P  |
# |   81 = Q  |
# |   82 = R  |
# |   83 = S  |
# |   84 = T  |
# |   85 = U  |
# |   86 = V  |
# |   87 = W  |
# |   88 = X  |
# |   89 = Y  |
# |   90 = Z  |
# |   91 = [  |
# |   92 = \  |
# |   93 = ]  |
# |   94 = ^  |
# |   95 = _  |
# |   96 = `  |
# |   97 = a  |
# |   98 = b  |
# |   99 = c  |
# |  100 = d  |
# |  101 = e  |
# |  102 = f  |
# |  103 = g  |
# |  104 = h  |
# |  105 = i  |
# |  106 = j  |
# |  107 = k  |
# |  108 = l  |
# |  109 = m  |
# |  110 = n  |
# |  111 = o  |
# |  112 = p  |
# |  113 = q  |
# |  114 = r  |
# |  115 = s  |
# |  116 = t  |
# |  117 = u  |
# |  118 = v  |
# |  119 = w  |
# |  120 = x  |
# |  121 = y  |
# |  122 = z  |
# |  123 = {  |
# |  124 = |  |
# |  125 = }  |
# |  126 = ~  |
# -------------
#

# Program:

begin
  bool printLine(int n) is
    int i = 0 ;
    while i < n do
      print "-" ;
      i = i + 1
    done ;
    println "" ;
    return true
  end

  bool printMap(int n) is
    print "|  " ;
    if n <100 then
      print " "
    else
      skip
    fi ;
    print n ;
    print " = " ;
    print chr n ;
    println "  |" ;
    return true
  end

  println "Ascii character lookup table:" ;
  bool r = call printLine(13) ;
  int num = ord ' ' ;
  while num < 127 do
    r = call printMap(num) ;
    num = num + 1
  done ;
  r = call printLine(13)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
asciiTable.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 1
4	.L.str0:
5		.asciz "-"
6	# length of .L.str1
7		.int 0
8	.L.str1:
9		.asciz ""
10	# length of .L.str2
11		.int 3
12	.L.str2:
13		.asciz "|  "
14	# length of .L.str3
15		.int 1
16	.L.str3:
17		.asciz " "
18	# length of .L.str4
19		.int 3
20	.L.str4:
21		.asciz " = "
22	# length of .L.str5
23		.int 3
24	.L.str5:
25		.asciz "  |"
26	# length of .L.str6
27		.int 29
28	.L.str6:
29		.asciz "Ascii character lookup table:"
30	.text
31	main:
32		pushq %rbp
33		# pushq {%rbx, %r12, %r13}
34		subq $24, %rsp
35		movq %rbx, (%rsp)
36		movq %r12, 8(%rsp)
37		movq %r13, 16(%rsp)
38		movq %rsp, %rbp
39		# Stack pointer unchanged, no stack allocated variables
40		# Stack pointer unchanged, no stack allocated arguments
41		leaq .L.str6(%rip), %rax
42		pushq %rax
43		popq %rax
44		movq %rax, %rax
45		movq %rax, %rdi
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _prints
48		call _println
49		# Stack pointer unchanged, no stack allocated arguments
50		movq $13, %rax
51		movq %rax, %rdi
52		call wacc_printLine
53		movq %rax, %r11
54		# Stack pointer unchanged, no stack allocated arguments
55		movq %r11, %rax
56		movq %rax, %r12
57		movq $32, %rax
58		movq %rax, %r13
59		jmp .L4
60	.L5:
61		# Stack pointer unchanged, no stack allocated arguments
62		movq %r13, %rax
63		movq %rax, %rdi
64		call wacc_printMap
65		movq %rax, %r11
66		# Stack pointer unchanged, no stack allocated arguments
67		movq %r11, %rax
68		movq %rax, %r12
69		movl %r13d, %eax
70		addl $1, %eax
71		jo _errOverflow
72		movslq %eax, %rax
73		pushq %rax
74		popq %rax
75		movq %rax, %rax
76		movq %rax, %r13
77	.L4:
78		cmpq $127, %r13
79		jl .L5
80		# Stack pointer unchanged, no stack allocated arguments
81		movq $13, %rax
82		movq %rax, %rdi
83		call wacc_printLine
84		movq %rax, %r11
85		# Stack pointer unchanged, no stack allocated arguments
86		movq %r11, %rax
87		movq %rax, %r12
88		# Stack pointer unchanged, no stack allocated variables
89		movq $0, %rax
90		# popq {%rbx, %r12, %r13}
91		movq (%rsp), %rbx
92		movq 8(%rsp), %r12
93		movq 16(%rsp), %r13
94		addq $24, %rsp
95		popq %rbp
96		ret
97	
98	wacc_printLine:
99		pushq %rbp
100		pushq %r12
101		movq %rsp, %rbp
102		# Stack pointer unchanged, no stack allocated variables
103		movq $0, %rax
104		movq %rax, %r12
105		jmp .L0
106	.L1:
107		pushq %rdi
108		# Set up R11 as a temporary second base pointer for the caller saved things
109		movq %rsp, %r11
110		# Stack pointer unchanged, no stack allocated arguments
111		leaq .L.str0(%rip), %rax
112		pushq %rax
113		popq %rax
114		movq %rax, %rax
115		movq %rax, %rdi
116		# statement primitives do not return results (but will clobber r0/rax)
117		call _prints
118		popq %rdi
119		movl %r12d, %eax
120		addl $1, %eax
121		jo _errOverflow
122		movslq %eax, %rax
123		pushq %rax
124		popq %rax
125		movq %rax, %rax
126		movq %rax, %r12
127	.L0:
128		cmpq %rdi, %r12
129		jl .L1
130		pushq %rdi
131		# Set up R11 as a temporary second base pointer for the caller saved things
132		movq %rsp, %r11
133		# Stack pointer unchanged, no stack allocated arguments
134		leaq .L.str1(%rip), %rax
135		pushq %rax
136		popq %rax
137		movq %rax, %rax
138		movq %rax, %rdi
139		# statement primitives do not return results (but will clobber r0/rax)
140		call _prints
141		call _println
142		popq %rdi
143		movq $1, %rax
144		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
145		movq %rbp, %rsp
146		popq %r12
147		popq %rbp
148		ret
149		# Stack pointer unchanged, no stack allocated variables
150		# 'ere be dragons: this is 100% dead code, functions always end in returns!
151	
152	wacc_printMap:
153		pushq %rbp
154		movq %rsp, %rbp
155		pushq %rdi
156		# Set up R11 as a temporary second base pointer for the caller saved things
157		movq %rsp, %r11
158		# Stack pointer unchanged, no stack allocated arguments
159		leaq .L.str2(%rip), %rax
160		pushq %rax
161		popq %rax
162		movq %rax, %rax
163		movq %rax, %rdi
164		# statement primitives do not return results (but will clobber r0/rax)
165		call _prints
166		popq %rdi
167		cmpq $100, %rdi
168		jl .L2
169		jmp .L3
170	.L2:
171		pushq %rdi
172		# Set up R11 as a temporary second base pointer for the caller saved things
173		movq %rsp, %r11
174		# Stack pointer unchanged, no stack allocated arguments
175		leaq .L.str3(%rip), %rax
176		pushq %rax
177		popq %rax
178		movq %rax, %rax
179		movq %rax, %rdi
180		# statement primitives do not return results (but will clobber r0/rax)
181		call _prints
182		popq %rdi
183	.L3:
184		pushq %rdi
185		# Set up R11 as a temporary second base pointer for the caller saved things
186		movq %rsp, %r11
187		# Stack pointer unchanged, no stack allocated arguments
188		movq %rdi, %rax
189		movq %rax, %rdi
190		# statement primitives do not return results (but will clobber r0/rax)
191		call _printi
192		popq %rdi
193		pushq %rdi
194		# Set up R11 as a temporary second base pointer for the caller saved things
195		movq %rsp, %r11
196		# Stack pointer unchanged, no stack allocated arguments
197		leaq .L.str4(%rip), %rax
198		pushq %rax
199		popq %rax
200		movq %rax, %rax
201		movq %rax, %rdi
202		# statement primitives do not return results (but will clobber r0/rax)
203		call _prints
204		popq %rdi
205		pushq %rdi
206		# Set up R11 as a temporary second base pointer for the caller saved things
207		movq %rsp, %r11
208		# Stack pointer unchanged, no stack allocated arguments
209		movq %rdi, %rax
210		testq $-128, %rax
211		cmovne %rax, %rsi
212		jne _errBadChar
213		pushq %rax
214		popq %rax
215		movq %rax, %rax
216		movq %rax, %rdi
217		# statement primitives do not return results (but will clobber r0/rax)
218		call _printc
219		popq %rdi
220		pushq %rdi
221		# Set up R11 as a temporary second base pointer for the caller saved things
222		movq %rsp, %r11
223		# Stack pointer unchanged, no stack allocated arguments
224		leaq .L.str5(%rip), %rax
225		pushq %rax
226		popq %rax
227		movq %rax, %rax
228		movq %rax, %rdi
229		# statement primitives do not return results (but will clobber r0/rax)
230		call _prints
231		call _println
232		popq %rdi
233		movq $1, %rax
234		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
235		movq %rbp, %rsp
236		popq %rbp
237		ret
238		# 'ere be dragons: this is 100% dead code, functions always end in returns!
239	
240	.section .rodata
241	# length of .L._prints_str0
242		.int 4
243	.L._prints_str0:
244		.asciz "%.*s"
245	.text
246	_prints:
247		pushq %rbp
248		movq %rsp, %rbp
249		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
250		andq $-16, %rsp
251		movq %rdi, %rdx
252		movl -4(%rdi), %esi
253		leaq .L._prints_str0(%rip), %rdi
254		# on x86, al represents the number of SIMD registers used as variadic arguments
255		movb $0, %al
256		call printf@plt
257		movq $0, %rdi
258		call fflush@plt
259		movq %rbp, %rsp
260		popq %rbp
261		ret
262	
263	.section .rodata
264	# length of .L._printi_str0
265		.int 2
266	.L._printi_str0:
267		.asciz "%d"
268	.text
269	_printi:
270		pushq %rbp
271		movq %rsp, %rbp
272		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
273		andq $-16, %rsp
274		movl %edi, %esi
275		leaq .L._printi_str0(%rip), %rdi
276		# on x86, al represents the number of SIMD registers used as variadic arguments
277		movb $0, %al
278		call printf@plt
279		movq $0, %rdi
280		call fflush@plt
281		movq %rbp, %rsp
282		popq %rbp
283		ret
284	
285	.section .rodata
286	# length of .L._printc_str0
287		.int 2
288	.L._printc_str0:
289		.asciz "%c"
290	.text
291	_printc:
292		pushq %rbp
293		movq %rsp, %rbp
294		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
295		andq $-16, %rsp
296		movb %dil, %sil
297		leaq .L._printc_str0(%rip), %rdi
298		# on x86, al represents the number of SIMD registers used as variadic arguments
299		movb $0, %al
300		call printf@plt
301		movq $0, %rdi
302		call fflush@plt
303		movq %rbp, %rsp
304		popq %rbp
305		ret
306	
307	.section .rodata
308	# length of .L._println_str0
309		.int 0
310	.L._println_str0:
311		.asciz ""
312	.text
313	_println:
314		pushq %rbp
315		movq %rsp, %rbp
316		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
317		andq $-16, %rsp
318		leaq .L._println_str0(%rip), %rdi
319		call puts@plt
320		movq $0, %rdi
321		call fflush@plt
322		movq %rbp, %rsp
323		popq %rbp
324		ret
325	
326	_exit:
327		pushq %rbp
328		movq %rsp, %rbp
329		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
330		andq $-16, %rsp
331		call exit@plt
332		movq %rbp, %rsp
333		popq %rbp
334		ret
335	
336	.section .rodata
337	# length of .L._errBadChar_str0
338		.int 50
339	.L._errBadChar_str0:
340		.asciz "fatal error: int %d is not ascii character 0-127 \n"
341	.text
342	_errBadChar:
343		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
344		andq $-16, %rsp
345		leaq .L._errBadChar_str0(%rip), %rdi
346		# on x86, al represents the number of SIMD registers used as variadic arguments
347		movb $0, %al
348		call printf@plt
349		movq $0, %rdi
350		call fflush@plt
351		movb $-1, %dil
352		call exit@plt
353	
354	.section .rodata
355	# length of .L._errOverflow_str0
356		.int 52
357	.L._errOverflow_str0:
358		.asciz "fatal error: integer overflow or underflow occurred\n"
359	.text
360	_errOverflow:
361		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
362		andq $-16, %rsp
363		leaq .L._errOverflow_str0(%rip), %rdi
364		call _prints
365		movb $-1, %dil
366		call exit@plt
===========================================================
-- Finished

