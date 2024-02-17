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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 1
5	.L.str0:
6		.asciz "-"
7	# length of .L.str1
8		.int 0
9	.L.str1:
10		.asciz ""
11	# length of .L.str2
12		.int 3
13	.L.str2:
14		.asciz "|  "
15	# length of .L.str3
16		.int 1
17	.L.str3:
18		.asciz " "
19	# length of .L.str4
20		.int 3
21	.L.str4:
22		.asciz " = "
23	# length of .L.str5
24		.int 3
25	.L.str5:
26		.asciz "  |"
27	# length of .L.str6
28		.int 29
29	.L.str6:
30		.asciz "Ascii character lookup table:"
31	.text
32	main:
33		push rbp
34		# push {rbx, r12, r13}
35		sub rsp, 24
36		mov qword ptr [rsp], rbx
37		mov qword ptr [rsp + 8], r12
38		mov qword ptr [rsp + 16], r13
39		mov rbp, rsp
40		# Stack pointer unchanged, no stack allocated variables
41		# Stack pointer unchanged, no stack allocated arguments
42		lea rax, [rip + .L.str6]
43		push rax
44		pop rax
45		mov rax, rax
46		mov rdi, rax
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _prints
49		call _println
50		# Stack pointer unchanged, no stack allocated arguments
51		mov rax, 13
52		mov rdi, rax
53		call wacc_printLine
54		mov r11, rax
55		# Stack pointer unchanged, no stack allocated arguments
56		mov rax, r11
57		mov r12, rax
58		mov rax, 32
59		mov r13, rax
60		jmp .L4
61	.L5:
62		# Stack pointer unchanged, no stack allocated arguments
63		mov rax, r13
64		mov rdi, rax
65		call wacc_printMap
66		mov r11, rax
67		# Stack pointer unchanged, no stack allocated arguments
68		mov rax, r11
69		mov r12, rax
70		mov eax, r13d
71		add eax, 1
72		jo _errOverflow
73		movsx rax, eax
74		push rax
75		pop rax
76		mov rax, rax
77		mov r13, rax
78	.L4:
79		cmp r13, 127
80		jl .L5
81		# Stack pointer unchanged, no stack allocated arguments
82		mov rax, 13
83		mov rdi, rax
84		call wacc_printLine
85		mov r11, rax
86		# Stack pointer unchanged, no stack allocated arguments
87		mov rax, r11
88		mov r12, rax
89		# Stack pointer unchanged, no stack allocated variables
90		mov rax, 0
91		# pop {rbx, r12, r13}
92		mov rbx, qword ptr [rsp]
93		mov r12, qword ptr [rsp + 8]
94		mov r13, qword ptr [rsp + 16]
95		add rsp, 24
96		pop rbp
97		ret
98	
99	wacc_printLine:
100		push rbp
101		push r12
102		mov rbp, rsp
103		# Stack pointer unchanged, no stack allocated variables
104		mov rax, 0
105		mov r12, rax
106		jmp .L0
107	.L1:
108		push rdi
109		# Set up R11 as a temporary second base pointer for the caller saved things
110		mov r11, rsp
111		# Stack pointer unchanged, no stack allocated arguments
112		lea rax, [rip + .L.str0]
113		push rax
114		pop rax
115		mov rax, rax
116		mov rdi, rax
117		# statement primitives do not return results (but will clobber r0/rax)
118		call _prints
119		pop rdi
120		mov eax, r12d
121		add eax, 1
122		jo _errOverflow
123		movsx rax, eax
124		push rax
125		pop rax
126		mov rax, rax
127		mov r12, rax
128	.L0:
129		cmp r12, rdi
130		jl .L1
131		push rdi
132		# Set up R11 as a temporary second base pointer for the caller saved things
133		mov r11, rsp
134		# Stack pointer unchanged, no stack allocated arguments
135		lea rax, [rip + .L.str1]
136		push rax
137		pop rax
138		mov rax, rax
139		mov rdi, rax
140		# statement primitives do not return results (but will clobber r0/rax)
141		call _prints
142		call _println
143		pop rdi
144		mov rax, 1
145		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
146		mov rsp, rbp
147		pop r12
148		pop rbp
149		ret
150		# Stack pointer unchanged, no stack allocated variables
151		# 'ere be dragons: this is 100% dead code, functions always end in returns!
152	
153	wacc_printMap:
154		push rbp
155		mov rbp, rsp
156		push rdi
157		# Set up R11 as a temporary second base pointer for the caller saved things
158		mov r11, rsp
159		# Stack pointer unchanged, no stack allocated arguments
160		lea rax, [rip + .L.str2]
161		push rax
162		pop rax
163		mov rax, rax
164		mov rdi, rax
165		# statement primitives do not return results (but will clobber r0/rax)
166		call _prints
167		pop rdi
168		cmp rdi, 100
169		jl .L2
170		jmp .L3
171	.L2:
172		push rdi
173		# Set up R11 as a temporary second base pointer for the caller saved things
174		mov r11, rsp
175		# Stack pointer unchanged, no stack allocated arguments
176		lea rax, [rip + .L.str3]
177		push rax
178		pop rax
179		mov rax, rax
180		mov rdi, rax
181		# statement primitives do not return results (but will clobber r0/rax)
182		call _prints
183		pop rdi
184	.L3:
185		push rdi
186		# Set up R11 as a temporary second base pointer for the caller saved things
187		mov r11, rsp
188		# Stack pointer unchanged, no stack allocated arguments
189		mov rax, rdi
190		mov rdi, rax
191		# statement primitives do not return results (but will clobber r0/rax)
192		call _printi
193		pop rdi
194		push rdi
195		# Set up R11 as a temporary second base pointer for the caller saved things
196		mov r11, rsp
197		# Stack pointer unchanged, no stack allocated arguments
198		lea rax, [rip + .L.str4]
199		push rax
200		pop rax
201		mov rax, rax
202		mov rdi, rax
203		# statement primitives do not return results (but will clobber r0/rax)
204		call _prints
205		pop rdi
206		push rdi
207		# Set up R11 as a temporary second base pointer for the caller saved things
208		mov r11, rsp
209		# Stack pointer unchanged, no stack allocated arguments
210		mov rax, rdi
211		test rax, -128
212		cmovne rsi, rax
213		jne _errBadChar
214		push rax
215		pop rax
216		mov rax, rax
217		mov rdi, rax
218		# statement primitives do not return results (but will clobber r0/rax)
219		call _printc
220		pop rdi
221		push rdi
222		# Set up R11 as a temporary second base pointer for the caller saved things
223		mov r11, rsp
224		# Stack pointer unchanged, no stack allocated arguments
225		lea rax, [rip + .L.str5]
226		push rax
227		pop rax
228		mov rax, rax
229		mov rdi, rax
230		# statement primitives do not return results (but will clobber r0/rax)
231		call _prints
232		call _println
233		pop rdi
234		mov rax, 1
235		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
236		mov rsp, rbp
237		pop rbp
238		ret
239		# 'ere be dragons: this is 100% dead code, functions always end in returns!
240	
241	.section .rodata
242	# length of .L._prints_str0
243		.int 4
244	.L._prints_str0:
245		.asciz "%.*s"
246	.text
247	_prints:
248		push rbp
249		mov rbp, rsp
250		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
251		and rsp, -16
252		mov rdx, rdi
253		mov esi, dword ptr [rdi - 4]
254		lea rdi, [rip + .L._prints_str0]
255		# on x86, al represents the number of SIMD registers used as variadic arguments
256		mov al, 0
257		call printf@plt
258		mov rdi, 0
259		call fflush@plt
260		mov rsp, rbp
261		pop rbp
262		ret
263	
264	.section .rodata
265	# length of .L._printi_str0
266		.int 2
267	.L._printi_str0:
268		.asciz "%d"
269	.text
270	_printi:
271		push rbp
272		mov rbp, rsp
273		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
274		and rsp, -16
275		mov esi, edi
276		lea rdi, [rip + .L._printi_str0]
277		# on x86, al represents the number of SIMD registers used as variadic arguments
278		mov al, 0
279		call printf@plt
280		mov rdi, 0
281		call fflush@plt
282		mov rsp, rbp
283		pop rbp
284		ret
285	
286	.section .rodata
287	# length of .L._printc_str0
288		.int 2
289	.L._printc_str0:
290		.asciz "%c"
291	.text
292	_printc:
293		push rbp
294		mov rbp, rsp
295		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
296		and rsp, -16
297		mov sil, dil
298		lea rdi, [rip + .L._printc_str0]
299		# on x86, al represents the number of SIMD registers used as variadic arguments
300		mov al, 0
301		call printf@plt
302		mov rdi, 0
303		call fflush@plt
304		mov rsp, rbp
305		pop rbp
306		ret
307	
308	.section .rodata
309	# length of .L._println_str0
310		.int 0
311	.L._println_str0:
312		.asciz ""
313	.text
314	_println:
315		push rbp
316		mov rbp, rsp
317		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
318		and rsp, -16
319		lea rdi, [rip + .L._println_str0]
320		call puts@plt
321		mov rdi, 0
322		call fflush@plt
323		mov rsp, rbp
324		pop rbp
325		ret
326	
327	_exit:
328		push rbp
329		mov rbp, rsp
330		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
331		and rsp, -16
332		call exit@plt
333		mov rsp, rbp
334		pop rbp
335		ret
336	
337	.section .rodata
338	# length of .L._errBadChar_str0
339		.int 50
340	.L._errBadChar_str0:
341		.asciz "fatal error: int %d is not ascii character 0-127 \n"
342	.text
343	_errBadChar:
344		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
345		and rsp, -16
346		lea rdi, [rip + .L._errBadChar_str0]
347		# on x86, al represents the number of SIMD registers used as variadic arguments
348		mov al, 0
349		call printf@plt
350		mov rdi, 0
351		call fflush@plt
352		mov dil, -1
353		call exit@plt
354	
355	.section .rodata
356	# length of .L._errOverflow_str0
357		.int 52
358	.L._errOverflow_str0:
359		.asciz "fatal error: integer overflow or underflow occurred\n"
360	.text
361	_errOverflow:
362		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
363		and rsp, -16
364		lea rdi, [rip + .L._errOverflow_str0]
365		call _prints
366		mov dil, -1
367		call exit@plt
===========================================================
-- Finished

