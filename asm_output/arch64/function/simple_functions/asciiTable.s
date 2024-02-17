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
0	.data
1	// length of .L.str0
2		.word 1
3	.L.str0:
4		.asciz "-"
5	// length of .L.str1
6		.word 0
7	.L.str1:
8		.asciz ""
9	// length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "|  "
13	// length of .L.str3
14		.word 1
15	.L.str3:
16		.asciz " "
17	// length of .L.str4
18		.word 3
19	.L.str4:
20		.asciz " = "
21	// length of .L.str5
22		.word 3
23	.L.str5:
24		.asciz "  |"
25	// length of .L.str6
26		.word 29
27	.L.str6:
28		.asciz "Ascii character lookup table:"
29	.align 4
30	.text
31	.global main
32	main:
33		// push {fp, lr}
34		stp fp, lr, [sp, #-16]!
35		// push {x19, x20}
36		stp x19, x20, [sp, #-16]!
37		mov fp, sp
38		// Stack pointer unchanged, no stack allocated variables
39		// Stack pointer unchanged, no stack allocated arguments
40		adrp x8, .L.str6
41		add x8, x8, :lo12:.L.str6
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		bl _println
51		// Stack pointer unchanged, no stack allocated arguments
52		mov x8, #13
53		mov x0, x8
54		bl wacc_printLine
55		mov x16, x0
56		// Stack pointer unchanged, no stack allocated arguments
57		mov x8, x16
58		mov x19, x8
59		mov x8, #32
60		mov x20, x8
61		b .L4
62	.L5:
63		// Stack pointer unchanged, no stack allocated arguments
64		mov x8, x20
65		mov x0, x8
66		bl wacc_printMap
67		mov x16, x0
68		// Stack pointer unchanged, no stack allocated arguments
69		mov x8, x16
70		mov x19, x8
71		adds w8, w20, #1
72		b.vs _errOverflow
73		sxtw x8, w8
74		// push {x8}
75		stp x8, xzr, [sp, #-16]!
76		// pop {x8}
77		ldp x8, xzr, [sp], #16
78		mov x8, x8
79		mov x20, x8
80	.L4:
81		cmp x20, #127
82		b.lt .L5
83		// Stack pointer unchanged, no stack allocated arguments
84		mov x8, #13
85		mov x0, x8
86		bl wacc_printLine
87		mov x16, x0
88		// Stack pointer unchanged, no stack allocated arguments
89		mov x8, x16
90		mov x19, x8
91		// Stack pointer unchanged, no stack allocated variables
92		mov x0, #0
93		// pop {x19, x20}
94		ldp x19, x20, [sp], #16
95		// pop {fp, lr}
96		ldp fp, lr, [sp], #16
97		ret
98	
99	wacc_printLine:
100		// push {fp, lr}
101		stp fp, lr, [sp, #-16]!
102		// push {x19}
103		stp x19, xzr, [sp, #-16]!
104		mov fp, sp
105		// Stack pointer unchanged, no stack allocated variables
106		mov x8, #0
107		mov x19, x8
108		b .L0
109	.L1:
110		// push {x0}
111		stp x0, xzr, [sp, #-16]!
112		// Set up X16 as a temporary second base pointer for the caller saved things
113		mov x16, sp
114		// Stack pointer unchanged, no stack allocated arguments
115		adrp x8, .L.str0
116		add x8, x8, :lo12:.L.str0
117		// push {x8}
118		stp x8, xzr, [sp, #-16]!
119		// pop {x8}
120		ldp x8, xzr, [sp], #16
121		mov x8, x8
122		mov x0, x8
123		// statement primitives do not return results (but will clobber r0/rax)
124		bl _prints
125		// pop {x0}
126		ldp x0, xzr, [sp], #16
127		adds w8, w19, #1
128		b.vs _errOverflow
129		sxtw x8, w8
130		// push {x8}
131		stp x8, xzr, [sp, #-16]!
132		// pop {x8}
133		ldp x8, xzr, [sp], #16
134		mov x8, x8
135		mov x19, x8
136	.L0:
137		cmp x19, x0
138		b.lt .L1
139		// push {x0}
140		stp x0, xzr, [sp, #-16]!
141		// Set up X16 as a temporary second base pointer for the caller saved things
142		mov x16, sp
143		// Stack pointer unchanged, no stack allocated arguments
144		adrp x8, .L.str1
145		add x8, x8, :lo12:.L.str1
146		// push {x8}
147		stp x8, xzr, [sp, #-16]!
148		// pop {x8}
149		ldp x8, xzr, [sp], #16
150		mov x8, x8
151		mov x0, x8
152		// statement primitives do not return results (but will clobber r0/rax)
153		bl _prints
154		bl _println
155		// pop {x0}
156		ldp x0, xzr, [sp], #16
157		mov x0, #1
158		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
159		mov sp, fp
160		// pop {x19}
161		ldp x19, xzr, [sp], #16
162		// pop {fp, lr}
163		ldp fp, lr, [sp], #16
164		ret
165		// Stack pointer unchanged, no stack allocated variables
166		// 'ere be dragons: this is 100% dead code, functions always end in returns!
167	
168	wacc_printMap:
169		// push {fp, lr}
170		stp fp, lr, [sp, #-16]!
171		mov fp, sp
172		// push {x0}
173		stp x0, xzr, [sp, #-16]!
174		// Set up X16 as a temporary second base pointer for the caller saved things
175		mov x16, sp
176		// Stack pointer unchanged, no stack allocated arguments
177		adrp x8, .L.str2
178		add x8, x8, :lo12:.L.str2
179		// push {x8}
180		stp x8, xzr, [sp, #-16]!
181		// pop {x8}
182		ldp x8, xzr, [sp], #16
183		mov x8, x8
184		mov x0, x8
185		// statement primitives do not return results (but will clobber r0/rax)
186		bl _prints
187		// pop {x0}
188		ldp x0, xzr, [sp], #16
189		cmp x0, #100
190		b.lt .L2
191		b .L3
192	.L2:
193		// push {x0}
194		stp x0, xzr, [sp, #-16]!
195		// Set up X16 as a temporary second base pointer for the caller saved things
196		mov x16, sp
197		// Stack pointer unchanged, no stack allocated arguments
198		adrp x8, .L.str3
199		add x8, x8, :lo12:.L.str3
200		// push {x8}
201		stp x8, xzr, [sp, #-16]!
202		// pop {x8}
203		ldp x8, xzr, [sp], #16
204		mov x8, x8
205		mov x0, x8
206		// statement primitives do not return results (but will clobber r0/rax)
207		bl _prints
208		// pop {x0}
209		ldp x0, xzr, [sp], #16
210	.L3:
211		// push {x0}
212		stp x0, xzr, [sp, #-16]!
213		// Set up X16 as a temporary second base pointer for the caller saved things
214		mov x16, sp
215		// Stack pointer unchanged, no stack allocated arguments
216		mov x8, x0
217		mov x0, x8
218		// statement primitives do not return results (but will clobber r0/rax)
219		bl _printi
220		// pop {x0}
221		ldp x0, xzr, [sp], #16
222		// push {x0}
223		stp x0, xzr, [sp, #-16]!
224		// Set up X16 as a temporary second base pointer for the caller saved things
225		mov x16, sp
226		// Stack pointer unchanged, no stack allocated arguments
227		adrp x8, .L.str4
228		add x8, x8, :lo12:.L.str4
229		// push {x8}
230		stp x8, xzr, [sp, #-16]!
231		// pop {x8}
232		ldp x8, xzr, [sp], #16
233		mov x8, x8
234		mov x0, x8
235		// statement primitives do not return results (but will clobber r0/rax)
236		bl _prints
237		// pop {x0}
238		ldp x0, xzr, [sp], #16
239		// push {x0}
240		stp x0, xzr, [sp, #-16]!
241		// Set up X16 as a temporary second base pointer for the caller saved things
242		mov x16, sp
243		// Stack pointer unchanged, no stack allocated arguments
244		mov x8, x0
245		tst x8, #0xffffffffffffff80
246		csel x1, x8, x1, ne
247		b.ne _errBadChar
248		// push {x8}
249		stp x8, xzr, [sp, #-16]!
250		// pop {x8}
251		ldp x8, xzr, [sp], #16
252		mov x8, x8
253		mov x0, x8
254		// statement primitives do not return results (but will clobber r0/rax)
255		bl _printc
256		// pop {x0}
257		ldp x0, xzr, [sp], #16
258		// push {x0}
259		stp x0, xzr, [sp, #-16]!
260		// Set up X16 as a temporary second base pointer for the caller saved things
261		mov x16, sp
262		// Stack pointer unchanged, no stack allocated arguments
263		adrp x8, .L.str5
264		add x8, x8, :lo12:.L.str5
265		// push {x8}
266		stp x8, xzr, [sp, #-16]!
267		// pop {x8}
268		ldp x8, xzr, [sp], #16
269		mov x8, x8
270		mov x0, x8
271		// statement primitives do not return results (but will clobber r0/rax)
272		bl _prints
273		bl _println
274		// pop {x0}
275		ldp x0, xzr, [sp], #16
276		mov x0, #1
277		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
278		mov sp, fp
279		// pop {fp, lr}
280		ldp fp, lr, [sp], #16
281		ret
282		// 'ere be dragons: this is 100% dead code, functions always end in returns!
283	
284	// length of .L._prints_str0
285		.word 4
286	.L._prints_str0:
287		.asciz "%.*s"
288	.align 4
289	_prints:
290		// push {lr}
291		stp lr, xzr, [sp, #-16]!
292		mov x2, x0
293		ldrsw x1, [x0, #-4]
294		adr x0, .L._prints_str0
295		bl printf
296		mov x0, #0
297		bl fflush
298		// pop {lr}
299		ldp lr, xzr, [sp], #16
300		ret
301	
302	// length of .L._printi_str0
303		.word 2
304	.L._printi_str0:
305		.asciz "%d"
306	.align 4
307	_printi:
308		// push {lr}
309		stp lr, xzr, [sp, #-16]!
310		mov x1, x0
311		adr x0, .L._printi_str0
312		bl printf
313		mov x0, #0
314		bl fflush
315		// pop {lr}
316		ldp lr, xzr, [sp], #16
317		ret
318	
319	// length of .L._printc_str0
320		.word 2
321	.L._printc_str0:
322		.asciz "%c"
323	.align 4
324	_printc:
325		// push {lr}
326		stp lr, xzr, [sp, #-16]!
327		mov x1, x0
328		adr x0, .L._printc_str0
329		bl printf
330		mov x0, #0
331		bl fflush
332		// pop {lr}
333		ldp lr, xzr, [sp], #16
334		ret
335	
336	// length of .L._println_str0
337		.word 0
338	.L._println_str0:
339		.asciz ""
340	.align 4
341	_println:
342		// push {lr}
343		stp lr, xzr, [sp, #-16]!
344		adr x0, .L._println_str0
345		bl puts
346		mov x0, #0
347		bl fflush
348		// pop {lr}
349		ldp lr, xzr, [sp], #16
350		ret
351	
352	// length of .L._errBadChar_str0
353		.word 50
354	.L._errBadChar_str0:
355		.asciz "fatal error: int %d is not ascii character 0-127 \n"
356	.align 4
357	_errBadChar:
358		adr x0, .L._errBadChar_str0
359		bl printf
360		mov x0, #0
361		bl fflush
362		mov w0, #-1
363		bl exit
364	
365	// length of .L._errOverflow_str0
366		.word 52
367	.L._errOverflow_str0:
368		.asciz "fatal error: integer overflow or underflow occurred\n"
369	.align 4
370	_errOverflow:
371		adr x0, .L._errOverflow_str0
372		bl _prints
373		mov w0, #-1
374		bl exit
===========================================================
-- Finished

