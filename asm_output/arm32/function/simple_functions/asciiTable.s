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
1	@ length of .L.str0
2		.word 1
3	.L.str0:
4		.asciz "-"
5	@ length of .L.str1
6		.word 0
7	.L.str1:
8		.asciz ""
9	@ length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "|  "
13	@ length of .L.str3
14		.word 1
15	.L.str3:
16		.asciz " "
17	@ length of .L.str4
18		.word 3
19	.L.str4:
20		.asciz " = "
21	@ length of .L.str5
22		.word 3
23	.L.str5:
24		.asciz "  |"
25	@ length of .L.str6
26		.word 29
27	.L.str6:
28		.asciz "Ascii character lookup table:"
29	.align 4
30	.text
31	.global main
32	main:
33		push {fp, lr}
34		push {r4, r5, r8, r10, r12}
35		mov fp, sp
36		@ Stack pointer unchanged, no stack allocated variables
37		@ Stack pointer unchanged, no stack allocated arguments
38		ldr r8, =.L.str6
39		push {r8}
40		pop {r8}
41		mov r8, r8
42		mov r0, r8
43		@ statement primitives do not return results (but will clobber r0/rax)
44		bl _prints
45		bl _println
46		@ Stack pointer unchanged, no stack allocated arguments
47		mov r8, #13
48		mov r0, r8
49		bl wacc_printLine
50		mov r12, r0
51		@ Stack pointer unchanged, no stack allocated arguments
52		mov r8, r12
53		mov r4, r8
54		mov r8, #32
55		mov r5, r8
56		b .L4
57	.L5:
58		@ Stack pointer unchanged, no stack allocated arguments
59		mov r8, r5
60		mov r0, r8
61		bl wacc_printMap
62		mov r12, r0
63		@ Stack pointer unchanged, no stack allocated arguments
64		mov r8, r12
65		mov r4, r8
66		adds r8, r5, #1
67		blvs _errOverflow
68		push {r8}
69		pop {r8}
70		mov r8, r8
71		mov r5, r8
72	.L4:
73		cmp r5, #127
74		blt .L5
75		@ Stack pointer unchanged, no stack allocated arguments
76		mov r8, #13
77		mov r0, r8
78		bl wacc_printLine
79		mov r12, r0
80		@ Stack pointer unchanged, no stack allocated arguments
81		mov r8, r12
82		mov r4, r8
83		@ Stack pointer unchanged, no stack allocated variables
84		mov r0, #0
85		pop {r4, r5, r8, r10, r12}
86		pop {fp, pc}
87	
88	wacc_printLine:
89		push {fp, lr}
90		push {r4}
91		mov fp, sp
92		@ Stack pointer unchanged, no stack allocated variables
93		mov r8, #0
94		mov r4, r8
95		b .L0
96	.L1:
97		push {r0}
98		@ Set up R12 as a temporary second base pointer for the caller saved things
99		mov r12, sp
100		@ Stack pointer unchanged, no stack allocated arguments
101		ldr r8, =.L.str0
102		push {r8}
103		pop {r8}
104		mov r8, r8
105		mov r0, r8
106		@ statement primitives do not return results (but will clobber r0/rax)
107		bl _prints
108		pop {r0}
109		adds r8, r4, #1
110		blvs _errOverflow
111		push {r8}
112		pop {r8}
113		mov r8, r8
114		mov r4, r8
115	.L0:
116		cmp r4, r0
117		blt .L1
118		push {r0}
119		@ Set up R12 as a temporary second base pointer for the caller saved things
120		mov r12, sp
121		@ Stack pointer unchanged, no stack allocated arguments
122		ldr r8, =.L.str1
123		push {r8}
124		pop {r8}
125		mov r8, r8
126		mov r0, r8
127		@ statement primitives do not return results (but will clobber r0/rax)
128		bl _prints
129		bl _println
130		pop {r0}
131		mov r0, #1
132		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
133		mov sp, fp
134		pop {r4}
135		pop {fp, pc}
136		@ Stack pointer unchanged, no stack allocated variables
137		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
138		@ the .ltorg here is used to generate the constant pool after every function
139		@ this is important for ensuring string literals and large integers are accessible!
140		.ltorg
141	
142	wacc_printMap:
143		push {fp, lr}
144		mov fp, sp
145		push {r0}
146		@ Set up R12 as a temporary second base pointer for the caller saved things
147		mov r12, sp
148		@ Stack pointer unchanged, no stack allocated arguments
149		ldr r8, =.L.str2
150		push {r8}
151		pop {r8}
152		mov r8, r8
153		mov r0, r8
154		@ statement primitives do not return results (but will clobber r0/rax)
155		bl _prints
156		pop {r0}
157		cmp r0, #100
158		blt .L2
159		b .L3
160	.L2:
161		push {r0}
162		@ Set up R12 as a temporary second base pointer for the caller saved things
163		mov r12, sp
164		@ Stack pointer unchanged, no stack allocated arguments
165		ldr r8, =.L.str3
166		push {r8}
167		pop {r8}
168		mov r8, r8
169		mov r0, r8
170		@ statement primitives do not return results (but will clobber r0/rax)
171		bl _prints
172		pop {r0}
173	.L3:
174		push {r0}
175		@ Set up R12 as a temporary second base pointer for the caller saved things
176		mov r12, sp
177		@ Stack pointer unchanged, no stack allocated arguments
178		mov r8, r0
179		mov r0, r8
180		@ statement primitives do not return results (but will clobber r0/rax)
181		bl _printi
182		pop {r0}
183		push {r0}
184		@ Set up R12 as a temporary second base pointer for the caller saved things
185		mov r12, sp
186		@ Stack pointer unchanged, no stack allocated arguments
187		ldr r8, =.L.str4
188		push {r8}
189		pop {r8}
190		mov r8, r8
191		mov r0, r8
192		@ statement primitives do not return results (but will clobber r0/rax)
193		bl _prints
194		pop {r0}
195		push {r0}
196		@ Set up R12 as a temporary second base pointer for the caller saved things
197		mov r12, sp
198		@ Stack pointer unchanged, no stack allocated arguments
199		mov r8, r0
200		ldr r9, =-128
201		tst r8, r9
202		movne r1, r8
203		blne _errBadChar
204		push {r8}
205		pop {r8}
206		mov r8, r8
207		mov r0, r8
208		@ statement primitives do not return results (but will clobber r0/rax)
209		bl _printc
210		pop {r0}
211		push {r0}
212		@ Set up R12 as a temporary second base pointer for the caller saved things
213		mov r12, sp
214		@ Stack pointer unchanged, no stack allocated arguments
215		ldr r8, =.L.str5
216		push {r8}
217		pop {r8}
218		mov r8, r8
219		mov r0, r8
220		@ statement primitives do not return results (but will clobber r0/rax)
221		bl _prints
222		bl _println
223		pop {r0}
224		mov r0, #1
225		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
226		mov sp, fp
227		pop {fp, pc}
228		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
229		@ the .ltorg here is used to generate the constant pool after every function
230		@ this is important for ensuring string literals and large integers are accessible!
231		.ltorg
232	
233	@ length of .L._prints_str0
234		.word 4
235	.L._prints_str0:
236		.asciz "%.*s"
237	.align 4
238	_prints:
239		push {fp, lr}
240		mov fp, sp
241		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
242		bic sp, sp, #0x7
243		mov r2, r0
244		ldr r1, [r0, #-4]
245		adr r0, .L._prints_str0
246		bl printf
247		mov r0, #0
248		bl fflush
249		mov sp, fp
250		pop {fp, pc}
251	
252	@ length of .L._printi_str0
253		.word 2
254	.L._printi_str0:
255		.asciz "%d"
256	.align 4
257	_printi:
258		push {fp, lr}
259		mov fp, sp
260		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
261		bic sp, sp, #0x7
262		mov r1, r0
263		adr r0, .L._printi_str0
264		bl printf
265		mov r0, #0
266		bl fflush
267		mov sp, fp
268		pop {fp, pc}
269	
270	@ length of .L._printc_str0
271		.word 2
272	.L._printc_str0:
273		.asciz "%c"
274	.align 4
275	_printc:
276		push {fp, lr}
277		mov fp, sp
278		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
279		bic sp, sp, #0x7
280		mov r1, r0
281		adr r0, .L._printc_str0
282		bl printf
283		mov r0, #0
284		bl fflush
285		mov sp, fp
286		pop {fp, pc}
287	
288	@ length of .L._println_str0
289		.word 0
290	.L._println_str0:
291		.asciz ""
292	.align 4
293	_println:
294		push {fp, lr}
295		mov fp, sp
296		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
297		bic sp, sp, #0x7
298		adr r0, .L._println_str0
299		bl puts
300		mov r0, #0
301		bl fflush
302		mov sp, fp
303		pop {fp, pc}
304	
305	_exit:
306		push {fp, lr}
307		mov fp, sp
308		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
309		bic sp, sp, #0x7
310		bl exit
311		mov sp, fp
312		pop {fp, pc}
313	
314	@ length of .L._errBadChar_str0
315		.word 50
316	.L._errBadChar_str0:
317		.asciz "fatal error: int %d is not ascii character 0-127 \n"
318	.align 4
319	_errBadChar:
320		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
321		bic sp, sp, #0x7
322		adr r0, .L._errBadChar_str0
323		bl printf
324		mov r0, #0
325		bl fflush
326		mov r0, #255
327		bl _exit
328	
329	@ length of .L._errOverflow_str0
330		.word 52
331	.L._errOverflow_str0:
332		.asciz "fatal error: integer overflow or underflow occurred\n"
333	.align 4
334	_errOverflow:
335		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
336		bic sp, sp, #0x7
337		adr r0, .L._errOverflow_str0
338		bl _prints
339		mov r0, #255
340		bl _exit
===========================================================
-- Finished

