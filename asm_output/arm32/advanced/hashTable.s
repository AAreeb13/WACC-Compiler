./valid/advanced/hashTable.wacc
calling the reference compiler on ./valid/advanced/hashTable.wacc
-- Test: hashTable.wacc

-- Uploaded file: 
---------------------------------------------------------------
# This program is interactive. We implement a hash table containing integers and we play with it.
# 
# A hash table is represented by an array of node lists. Each node in a node list is represented 
# by pair(int, pair). The first element of the pair is the integer at that node, the second element 
# is the pointer to the next node (or null if no more node). 
# 
# Integers those mapped to the same index are stored on the list (in any order) in that index.

begin

	######################### Functions for Hash Table Interface ###############################
	
	# Given a hash table, initialise it. Return true.
	bool init(pair(int, pair)[] table) is
		int length = len table ;
		int i = 0 ;
		while i < length do
			table[i] = null ; 
			i = i + 1
		done ;
		return true
	end

	# Returns true if and only if the given hash table contains x.
	bool contain(pair(int, pair)[] table, int x) is
		int index = call calculateIndex(table, x) ;
		pair(int, pair) node = call findNode(table[index], x) ;
		return node != null
	end
	
	# Insert the given x into the hash table if it does not already contain x.
	# Returns true if and only if the table does not already contain x.
	bool insertIfNotContain(pair(int, pair)[] table, int x) is
		int index = call calculateIndex(table, x) ;
		pair(int, pair) node = call findNode(table[index], x) ;
		if node != null then
			# Already contain it. Do nothing.
			return false
		else
			# Insert in the front of the list.
			pair(int, pair) p = newpair(x, table[index]) ;
			table[index] = p ;
			return true 
		fi
	end
	
	# Remove the given x from the hash table. Returns true if an only if the table contains x. 
	# Otherwise, do nothing and returns false.
	bool remove(pair(int, pair)[] table, int x) is
		int index = call calculateIndex(table, x) ;
		pair(int, pair) node = call findNode(table[index], x) ;
		if node == null then
			# Not found x. Just return false.
			return false
		else
			# Found x, have to remove the node.
			table[index] = call removeNode(table[index], node) ;
			return true
		fi
	end
	
	# Remove all nodes from the table. Returns true.
	bool removeAll(pair(int, pair)[] table) is
		int length = len table ;
		int i = 0 ;
		while i < length do
			pair(int, pair) p = table[i] ;
			while p != null do
				pair(int, pair) p2 = snd p ;
				free p ;
				p = p2
			done ;
			table[i] = null ;
			i = i + 1
		done ; 
		return true
	end
	
	# Count the number of integers in the table and return it.
	int count(pair(int, pair)[] table) is
		int length = len table ;
		int sum = 0 ;
		int i = 0 ;
		while i < length do
			int subSum = call countNodes(table[i]) ;
			sum = sum + subSum ;
			i = i + 1
		done ;
		return sum
	end
	
	# Print all the integers inside the table, separated by a space and ended with a newline. Returns true.
	bool printAll(pair(int, pair)[] table) is
		int length = len table ;
		int i = 0 ;
		while i < length do
      bool result = call printAllNodes(table[i]) ;
			i = i + 1
		done ;
		println "" ;
		return true
	end
		
	# A helper function.
	# Given a hash table and an integer, calculate the index of the integer in the table.
	int calculateIndex(pair(int, pair)[] table, int x) is
		int length = len table ;
		return x % length
	end
	
	# A helper function.
	# Given a head of a chain of nodes, returns the first node containing the value x.
	# Returns null if no such node.
	pair(int, pair) findNode(pair(int, pair) head, int x) is
		while head != null do
			int y = fst head ;
			if y == x then
				return head
			else
				head = snd head
			fi 
		done ;
		return null
	end

	# A helper function.
	# Given a list of nodes and a node to remove, remove that node from the 
	# list and return the new list.
	pair(int, pair) removeNode(pair(int, pair) head, pair(int, pair) toRemove) is
		if head == null then
			# Should not happen actually.
			return null
		else
			if head == toRemove then
				# Save the new head.
				head = snd head ;
				
				# Deallocate the memory of the old node.
				free toRemove ;
				
				# Return the new head.
				return head
			else
				# Not this node, recursive.
				pair(int, pair) tail = snd head ;
				snd head = call removeNode(tail, toRemove) ;
				return head
			fi
		fi
	end

	# A helper function.
	# Given a list of nodes, count how many nodes there are.
	int countNodes(pair(int, pair) head) is 
		int sum = 0 ;
		while head != null do
			sum = sum + 1 ;
			head = snd head
		done ;
		return sum
	end

	# A helper function.
	# Given a list of nodes, print each integer in the node followed by a space. Returns true.
	bool printAllNodes(pair(int, pair) head) is
    while head != null do
			int x = fst head ;
			print x ;
			print ' ' ;
			head = snd head
		done ;
		return true
	end

	######################### Functions for Command Line Interface ###############################
	
	# Print the menu and ask to choose. Returns a valid decision.
	char printMenu() is
		println "===========================================" ;
		println "========== Hash Table Program =============" ;
		println "===========================================" ;
		println "=                                         =" ;
		println "= Please choose the following options:    =" ;
		println "=                                         =" ;
		println "= a: insert an integer                    =" ;
		println "= b: find an integer                      =" ;
		println "= c: count the integers                   =" ;
		println "= d: print all integers                   =" ;
		println "= e: remove an integer                    =" ;
		println "= f: remove all integers                  =" ;
		println "= g: exit                                 =" ;
		println "=                                         =" ;
		println "===========================================" ;
		
		int minChoice = ord 'a' ;
		int maxChoice = ord 'g' ;
		
		while true do
			print   "Your decision: " ;
			char d = '\0' ;
			read d ;
			int dInt = ord d ;
			if minChoice <= dInt && dInt <= maxChoice then
				return d
			else 
				print "You have entered: " ;
				print d ;
				println " which is invalid, please try again."
			fi
		done ;
		# The compiler is not smart enough to know that this never reaches. 
		# We have to add a return statement here.
		return '\0'
	end
	
	# Print out the question, and then read an integer. After that print the integer back and return it.
	int askForInt(string message) is
		print message ;
		int x = 0 ;
		read x ;
		print "You have entered: " ;
		println x ;
		return x
	end
	
	# Handle menu insert. Returns true.
	bool handleMenuInsert(pair(int, pair)[] table) is
		int x = call askForInt("Please enter an integer to insert: ") ;
		bool notContain = call insertIfNotContain(table, x) ;
		if notContain then
			println "Successfully insert it. The integer is new." 
		else 
			println "The integer is already there. No insertion is made."
		fi ;
		return true		
	end
	
	# Handle menu find. Returns true.
	bool handleMenuFind(pair(int, pair)[] table) is
		int x = call askForInt("Please enter an integer to find: ") ;
		bool find = call contain(table, x) ;
		if find then
			println "Find the integer." 
		else 
			println "The integer is not found."
		fi ;
		return true		
	end
	
	# Handle menu count. Returns true.
	bool handleMenuCount(pair(int, pair)[] table) is
		int size = call count(table) ;
		if size == 1 then
			println "There is only 1 integer."
		else
			print "There are " ;
			print size ;
			println " integers."
		fi ; 
		return true
	end
	
	# Handle menu print. Returns true.
	bool handleMenuPrint(pair(int, pair)[] table) is
		print "Here are the integers: " ;
		bool junk = call printAll(table) ;
		return true
	end
	
	# Handle menu remove. Returns true.
	bool handleMenuRemove(pair(int, pair)[] table) is
		int x = call askForInt("Please enter an integer to remove: ") ;
		bool find = call remove(table, x) ;
		if find then
			println "The integer has been removed." 
		else 
			println "The integer is not found."
		fi ;
		return true		
	end
	
	# Handle menu remove all. Returns true.
	bool handleMenuRemoveAll(pair(int, pair)[] table) is
		bool junk = call removeAll(table) ; 
		println "All integers have been removed." ; 
		return true
	end
	
	################################# The main function ########################################
	# Our hash table of size 13.
	pair(int, pair)[] table = [null, null, null, null, null, null, null, null, null, null, null, null, null] ;
	bool junk = call init(table) ;
	
	bool continue = true ;
	while continue do
		char choice = call printMenu() ;
		if choice == 'a' then
			bool result = call handleMenuInsert(table)
		else if choice == 'b' then
			bool result = call handleMenuFind(table)
		else if choice == 'c' then
			bool result = call handleMenuCount(table)
		else if choice == 'd' then
			bool result = call handleMenuPrint(table)
		else if choice == 'e' then
			bool result = call handleMenuRemove(table)
		else if choice == 'f' then
			bool result = call handleMenuRemoveAll(table)
		else if choice == 'g' then
			println "Goodbye Human" ;
			continue = false
		else
			# Should not happen.
			print "Error: unknown choice (" ;
			print choice ;
			println ")" ;
			exit -1
		fi fi fi fi fi fi fi
	done
	
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
hashTable.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 0
3	.L.str0:
4		.asciz ""
5	@ length of .L.str1
6		.word 43
7	.L.str1:
8		.asciz "==========================================="
9	@ length of .L.str2
10		.word 43
11	.L.str2:
12		.asciz "========== Hash Table Program ============="
13	@ length of .L.str3
14		.word 43
15	.L.str3:
16		.asciz "=                                         ="
17	@ length of .L.str4
18		.word 43
19	.L.str4:
20		.asciz "= Please choose the following options:    ="
21	@ length of .L.str5
22		.word 43
23	.L.str5:
24		.asciz "= a: insert an integer                    ="
25	@ length of .L.str6
26		.word 43
27	.L.str6:
28		.asciz "= b: find an integer                      ="
29	@ length of .L.str7
30		.word 43
31	.L.str7:
32		.asciz "= c: count the integers                   ="
33	@ length of .L.str8
34		.word 43
35	.L.str8:
36		.asciz "= d: print all integers                   ="
37	@ length of .L.str9
38		.word 43
39	.L.str9:
40		.asciz "= e: remove an integer                    ="
41	@ length of .L.str10
42		.word 43
43	.L.str10:
44		.asciz "= f: remove all integers                  ="
45	@ length of .L.str11
46		.word 43
47	.L.str11:
48		.asciz "= g: exit                                 ="
49	@ length of .L.str12
50		.word 15
51	.L.str12:
52		.asciz "Your decision: "
53	@ length of .L.str13
54		.word 18
55	.L.str13:
56		.asciz "You have entered: "
57	@ length of .L.str14
58		.word 36
59	.L.str14:
60		.asciz " which is invalid, please try again."
61	@ length of .L.str15
62		.word 35
63	.L.str15:
64		.asciz "Please enter an integer to insert: "
65	@ length of .L.str16
66		.word 51
67	.L.str16:
68		.asciz "The integer is already there. No insertion is made."
69	@ length of .L.str17
70		.word 43
71	.L.str17:
72		.asciz "Successfully insert it. The integer is new."
73	@ length of .L.str18
74		.word 33
75	.L.str18:
76		.asciz "Please enter an integer to find: "
77	@ length of .L.str19
78		.word 25
79	.L.str19:
80		.asciz "The integer is not found."
81	@ length of .L.str20
82		.word 17
83	.L.str20:
84		.asciz "Find the integer."
85	@ length of .L.str21
86		.word 10
87	.L.str21:
88		.asciz "There are "
89	@ length of .L.str22
90		.word 10
91	.L.str22:
92		.asciz " integers."
93	@ length of .L.str23
94		.word 24
95	.L.str23:
96		.asciz "There is only 1 integer."
97	@ length of .L.str24
98		.word 23
99	.L.str24:
100		.asciz "Here are the integers: "
101	@ length of .L.str25
102		.word 35
103	.L.str25:
104		.asciz "Please enter an integer to remove: "
105	@ length of .L.str26
106		.word 29
107	.L.str26:
108		.asciz "The integer has been removed."
109	@ length of .L.str27
110		.word 31
111	.L.str27:
112		.asciz "All integers have been removed."
113	@ length of .L.str28
114		.word 23
115	.L.str28:
116		.asciz "Error: unknown choice ("
117	@ length of .L.str29
118		.word 1
119	.L.str29:
120		.asciz ")"
121	@ length of .L.str30
122		.word 13
123	.L.str30:
124		.asciz "Goodbye Human"
125	.align 4
126	.text
127	.global main
128	main:
129		push {fp, lr}
130		push {r4, r5, r6, r7, r8, r10, r12}
131		mov fp, sp
132		@ Stack pointer unchanged, no stack allocated variables
133		@ 13 element array
134		push {r0}
135		mov r0, #56
136		bl _malloc
137		mov r12, r0
138		pop {r0}
139		@ array pointers are shifted forwards by 4 bytes (to account for size)
140		add r12, r12, #4
141		mov r8, #13
142		str r8, [r12, #-4]
143		mov r8, #0
144		str r8, [r12, #0]
145		mov r8, #0
146		str r8, [r12, #4]
147		mov r8, #0
148		str r8, [r12, #8]
149		mov r8, #0
150		str r8, [r12, #12]
151		mov r8, #0
152		str r8, [r12, #16]
153		mov r8, #0
154		str r8, [r12, #20]
155		mov r8, #0
156		str r8, [r12, #24]
157		mov r8, #0
158		str r8, [r12, #28]
159		mov r8, #0
160		str r8, [r12, #32]
161		mov r8, #0
162		str r8, [r12, #36]
163		mov r8, #0
164		str r8, [r12, #40]
165		mov r8, #0
166		str r8, [r12, #44]
167		mov r8, #0
168		str r8, [r12, #48]
169		mov r8, r12
170		mov r4, r8
171		push {r0}
172		@ Set up R12 as a temporary second base pointer for the caller saved things
173		mov r12, sp
174		@ Stack pointer unchanged, no stack allocated arguments
175		mov r8, r4
176		mov r0, r8
177		bl wacc_init
178		mov r12, r0
179		@ Stack pointer unchanged, no stack allocated arguments
180		pop {r0}
181		mov r8, r12
182		mov r5, r8
183		mov r8, #1
184		mov r6, r8
185		b .L39
186	.L40:
187		@ Stack pointer unchanged, no stack allocated variables
188		push {r0}
189		@ Set up R12 as a temporary second base pointer for the caller saved things
190		mov r12, sp
191		@ Stack pointer unchanged, no stack allocated arguments
192		bl wacc_printMenu
193		mov r12, r0
194		@ Stack pointer unchanged, no stack allocated arguments
195		pop {r0}
196		mov r8, r12
197		mov r7, r8
198		cmp r7, #97
199		beq .L41
200		cmp r7, #98
201		beq .L43
202		cmp r7, #99
203		beq .L45
204		cmp r7, #100
205		beq .L47
206		cmp r7, #101
207		beq .L49
208		cmp r7, #102
209		beq .L51
210		cmp r7, #103
211		beq .L53
212		push {r0}
213		@ Set up R12 as a temporary second base pointer for the caller saved things
214		mov r12, sp
215		@ Stack pointer unchanged, no stack allocated arguments
216		ldr r8, =.L.str28
217		push {r8}
218		pop {r8}
219		mov r8, r8
220		mov r0, r8
221		@ statement primitives do not return results (but will clobber r0/rax)
222		bl _prints
223		pop {r0}
224		push {r0}
225		@ Set up R12 as a temporary second base pointer for the caller saved things
226		mov r12, sp
227		@ Stack pointer unchanged, no stack allocated arguments
228		mov r8, r7
229		mov r0, r8
230		@ statement primitives do not return results (but will clobber r0/rax)
231		bl _printc
232		pop {r0}
233		push {r0}
234		@ Set up R12 as a temporary second base pointer for the caller saved things
235		mov r12, sp
236		@ Stack pointer unchanged, no stack allocated arguments
237		ldr r8, =.L.str29
238		push {r8}
239		pop {r8}
240		mov r8, r8
241		mov r0, r8
242		@ statement primitives do not return results (but will clobber r0/rax)
243		bl _prints
244		bl _println
245		pop {r0}
246		push {r0}
247		@ Set up R12 as a temporary second base pointer for the caller saved things
248		mov r12, sp
249		@ Stack pointer unchanged, no stack allocated arguments
250		ldr r8, =-1
251		mov r8, r8
252		mov r0, r8
253		@ statement primitives do not return results (but will clobber r0/rax)
254		bl _exit
255		pop {r0}
256		b .L54
257	.L53:
258		push {r0}
259		@ Set up R12 as a temporary second base pointer for the caller saved things
260		mov r12, sp
261		@ Stack pointer unchanged, no stack allocated arguments
262		ldr r8, =.L.str30
263		push {r8}
264		pop {r8}
265		mov r8, r8
266		mov r0, r8
267		@ statement primitives do not return results (but will clobber r0/rax)
268		bl _prints
269		bl _println
270		pop {r0}
271		mov r8, #0
272		mov r6, r8
273	.L54:
274		b .L52
275	.L51:
276		@ Stack pointer unchanged, no stack allocated variables
277		push {r0}
278		@ Set up R12 as a temporary second base pointer for the caller saved things
279		mov r12, sp
280		@ Stack pointer unchanged, no stack allocated arguments
281		mov r8, r4
282		mov r0, r8
283		bl wacc_handleMenuRemoveAll
284		mov r12, r0
285		@ Stack pointer unchanged, no stack allocated arguments
286		pop {r0}
287		mov r8, r12
288		mov r0, r8
289		@ Stack pointer unchanged, no stack allocated variables
290	.L52:
291		b .L50
292	.L49:
293		@ Stack pointer unchanged, no stack allocated variables
294		push {r0}
295		@ Set up R12 as a temporary second base pointer for the caller saved things
296		mov r12, sp
297		@ Stack pointer unchanged, no stack allocated arguments
298		mov r8, r4
299		mov r0, r8
300		bl wacc_handleMenuRemove
301		mov r12, r0
302		@ Stack pointer unchanged, no stack allocated arguments
303		pop {r0}
304		mov r8, r12
305		mov r0, r8
306		@ Stack pointer unchanged, no stack allocated variables
307	.L50:
308		b .L48
309	.L47:
310		@ Stack pointer unchanged, no stack allocated variables
311		push {r0}
312		@ Set up R12 as a temporary second base pointer for the caller saved things
313		mov r12, sp
314		@ Stack pointer unchanged, no stack allocated arguments
315		mov r8, r4
316		mov r0, r8
317		bl wacc_handleMenuPrint
318		mov r12, r0
319		@ Stack pointer unchanged, no stack allocated arguments
320		pop {r0}
321		mov r8, r12
322		mov r0, r8
323		@ Stack pointer unchanged, no stack allocated variables
324	.L48:
325		b .L46
326	.L45:
327		@ Stack pointer unchanged, no stack allocated variables
328		push {r0}
329		@ Set up R12 as a temporary second base pointer for the caller saved things
330		mov r12, sp
331		@ Stack pointer unchanged, no stack allocated arguments
332		mov r8, r4
333		mov r0, r8
334		bl wacc_handleMenuCount
335		mov r12, r0
336		@ Stack pointer unchanged, no stack allocated arguments
337		pop {r0}
338		mov r8, r12
339		mov r0, r8
340		@ Stack pointer unchanged, no stack allocated variables
341	.L46:
342		b .L44
343	.L43:
344		@ Stack pointer unchanged, no stack allocated variables
345		push {r0}
346		@ Set up R12 as a temporary second base pointer for the caller saved things
347		mov r12, sp
348		@ Stack pointer unchanged, no stack allocated arguments
349		mov r8, r4
350		mov r0, r8
351		bl wacc_handleMenuFind
352		mov r12, r0
353		@ Stack pointer unchanged, no stack allocated arguments
354		pop {r0}
355		mov r8, r12
356		mov r0, r8
357		@ Stack pointer unchanged, no stack allocated variables
358	.L44:
359		b .L42
360	.L41:
361		@ Stack pointer unchanged, no stack allocated variables
362		push {r0}
363		@ Set up R12 as a temporary second base pointer for the caller saved things
364		mov r12, sp
365		@ Stack pointer unchanged, no stack allocated arguments
366		mov r8, r4
367		mov r0, r8
368		bl wacc_handleMenuInsert
369		mov r12, r0
370		@ Stack pointer unchanged, no stack allocated arguments
371		pop {r0}
372		mov r8, r12
373		mov r0, r8
374		@ Stack pointer unchanged, no stack allocated variables
375	.L42:
376		@ Stack pointer unchanged, no stack allocated variables
377	.L39:
378		cmp r6, #1
379		beq .L40
380		@ Stack pointer unchanged, no stack allocated variables
381		mov r0, #0
382		pop {r4, r5, r6, r7, r8, r10, r12}
383		pop {fp, pc}
384	
385	wacc_init:
386		push {fp, lr}
387		push {r4, r5}
388		mov fp, sp
389		@ Stack pointer unchanged, no stack allocated variables
390		ldr r8, [r0, #-4]
391		mov r8, r8
392		mov r4, r8
393		mov r8, #0
394		mov r5, r8
395		b .L0
396	.L1:
397		mov r10, r5
398		mov r8, #0
399		mov r3, r0
400		bl _arrStore4
401		adds r8, r5, #1
402		blvs _errOverflow
403		push {r8}
404		pop {r8}
405		mov r8, r8
406		mov r5, r8
407	.L0:
408		cmp r5, r4
409		blt .L1
410		mov r0, #1
411		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
412		mov sp, fp
413		pop {r4, r5}
414		pop {fp, pc}
415		@ Stack pointer unchanged, no stack allocated variables
416		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
417		@ the .ltorg here is used to generate the constant pool after every function
418		@ this is important for ensuring string literals and large integers are accessible!
419		.ltorg
420	
421	wacc_contain:
422		push {fp, lr}
423		push {r4, r5}
424		mov fp, sp
425		@ Stack pointer unchanged, no stack allocated variables
426		push {r0, r1}
427		@ Set up R12 as a temporary second base pointer for the caller saved things
428		mov r12, sp
429		@ Stack pointer unchanged, no stack allocated arguments
430		mov r8, r0
431		mov r0, r8
432		mov r8, r1
433		mov r1, r8
434		bl wacc_calculateIndex
435		mov r12, r0
436		@ Stack pointer unchanged, no stack allocated arguments
437		pop {r0, r1}
438		mov r8, r12
439		mov r4, r8
440		push {r0, r1}
441		@ Set up R12 as a temporary second base pointer for the caller saved things
442		mov r12, sp
443		@ Stack pointer unchanged, no stack allocated arguments
444		mov r10, r4
445		mov r3, r0
446		bl _arrLoad4
447		mov r8, r3
448		mov r8, r8
449		mov r0, r8
450		mov r8, r1
451		mov r1, r8
452		bl wacc_findNode
453		mov r12, r0
454		@ Stack pointer unchanged, no stack allocated arguments
455		pop {r0, r1}
456		mov r8, r12
457		mov r5, r8
458		cmp r5, #0
459		movne r8, #1
460		moveq r8, #0
461		push {r8}
462		pop {r0}
463		mov r0, r0
464		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
465		mov sp, fp
466		pop {r4, r5}
467		pop {fp, pc}
468		@ Stack pointer unchanged, no stack allocated variables
469		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
470		@ the .ltorg here is used to generate the constant pool after every function
471		@ this is important for ensuring string literals and large integers are accessible!
472		.ltorg
473	
474	wacc_insertIfNotContain:
475		push {fp, lr}
476		push {r4, r5, r6}
477		mov fp, sp
478		@ Stack pointer unchanged, no stack allocated variables
479		push {r0, r1}
480		@ Set up R12 as a temporary second base pointer for the caller saved things
481		mov r12, sp
482		@ Stack pointer unchanged, no stack allocated arguments
483		mov r8, r0
484		mov r0, r8
485		mov r8, r1
486		mov r1, r8
487		bl wacc_calculateIndex
488		mov r12, r0
489		@ Stack pointer unchanged, no stack allocated arguments
490		pop {r0, r1}
491		mov r8, r12
492		mov r4, r8
493		push {r0, r1}
494		@ Set up R12 as a temporary second base pointer for the caller saved things
495		mov r12, sp
496		@ Stack pointer unchanged, no stack allocated arguments
497		mov r10, r4
498		mov r3, r0
499		bl _arrLoad4
500		mov r8, r3
501		mov r8, r8
502		mov r0, r8
503		mov r8, r1
504		mov r1, r8
505		bl wacc_findNode
506		mov r12, r0
507		@ Stack pointer unchanged, no stack allocated arguments
508		pop {r0, r1}
509		mov r8, r12
510		mov r5, r8
511		cmp r5, #0
512		bne .L2
513		@ Stack pointer unchanged, no stack allocated variables
514		push {r0, r1}
515		mov r0, #8
516		bl _malloc
517		mov r12, r0
518		pop {r0, r1}
519		mov r8, r1
520		str r8, [r12, #0]
521		mov r10, r4
522		mov r3, r0
523		bl _arrLoad4
524		mov r8, r3
525		mov r8, r8
526		str r8, [r12, #4]
527		mov r8, r12
528		mov r6, r8
529		mov r10, r4
530		mov r8, r6
531		mov r3, r0
532		bl _arrStore4
533		mov r0, #1
534		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
535		mov sp, fp
536		pop {r4, r5, r6}
537		pop {fp, pc}
538		@ Stack pointer unchanged, no stack allocated variables
539		b .L3
540	.L2:
541		mov r0, #0
542		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
543		mov sp, fp
544		pop {r4, r5, r6}
545		pop {fp, pc}
546	.L3:
547		@ Stack pointer unchanged, no stack allocated variables
548		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
549		@ the .ltorg here is used to generate the constant pool after every function
550		@ this is important for ensuring string literals and large integers are accessible!
551		.ltorg
552	
553	wacc_remove:
554		push {fp, lr}
555		push {r4, r5}
556		mov fp, sp
557		@ Stack pointer unchanged, no stack allocated variables
558		push {r0, r1}
559		@ Set up R12 as a temporary second base pointer for the caller saved things
560		mov r12, sp
561		@ Stack pointer unchanged, no stack allocated arguments
562		mov r8, r0
563		mov r0, r8
564		mov r8, r1
565		mov r1, r8
566		bl wacc_calculateIndex
567		mov r12, r0
568		@ Stack pointer unchanged, no stack allocated arguments
569		pop {r0, r1}
570		mov r8, r12
571		mov r4, r8
572		push {r0, r1}
573		@ Set up R12 as a temporary second base pointer for the caller saved things
574		mov r12, sp
575		@ Stack pointer unchanged, no stack allocated arguments
576		mov r10, r4
577		mov r3, r0
578		bl _arrLoad4
579		mov r8, r3
580		mov r8, r8
581		mov r0, r8
582		mov r8, r1
583		mov r1, r8
584		bl wacc_findNode
585		mov r12, r0
586		@ Stack pointer unchanged, no stack allocated arguments
587		pop {r0, r1}
588		mov r8, r12
589		mov r5, r8
590		cmp r5, #0
591		beq .L4
592		push {r0, r1}
593		@ Set up R12 as a temporary second base pointer for the caller saved things
594		mov r12, sp
595		@ Stack pointer unchanged, no stack allocated arguments
596		mov r10, r4
597		mov r3, r0
598		bl _arrLoad4
599		mov r8, r3
600		mov r8, r8
601		mov r0, r8
602		mov r8, r5
603		mov r1, r8
604		bl wacc_removeNode
605		mov r12, r0
606		@ Stack pointer unchanged, no stack allocated arguments
607		pop {r0, r1}
608		mov r10, r4
609		mov r8, r12
610		mov r3, r0
611		bl _arrStore4
612		mov r0, #1
613		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
614		mov sp, fp
615		pop {r4, r5}
616		pop {fp, pc}
617		b .L5
618	.L4:
619		mov r0, #0
620		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
621		mov sp, fp
622		pop {r4, r5}
623		pop {fp, pc}
624	.L5:
625		@ Stack pointer unchanged, no stack allocated variables
626		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
627		@ the .ltorg here is used to generate the constant pool after every function
628		@ this is important for ensuring string literals and large integers are accessible!
629		.ltorg
630	
631	wacc_removeAll:
632		push {fp, lr}
633		push {r4, r5, r6, r7}
634		mov fp, sp
635		@ Stack pointer unchanged, no stack allocated variables
636		ldr r8, [r0, #-4]
637		mov r8, r8
638		mov r4, r8
639		mov r8, #0
640		mov r5, r8
641		b .L6
642	.L7:
643		@ Stack pointer unchanged, no stack allocated variables
644		mov r10, r5
645		mov r3, r0
646		bl _arrLoad4
647		mov r8, r3
648		mov r8, r8
649		mov r6, r8
650		b .L8
651	.L9:
652		@ Stack pointer unchanged, no stack allocated variables
653		cmp r6, #0
654		bleq _errNull
655		ldr r8, [r6, #4]
656		mov r8, r8
657		mov r7, r8
658		push {r0}
659		@ Set up R12 as a temporary second base pointer for the caller saved things
660		mov r12, sp
661		@ Stack pointer unchanged, no stack allocated arguments
662		mov r8, r6
663		mov r0, r8
664		@ statement primitives do not return results (but will clobber r0/rax)
665		bl _freepair
666		pop {r0}
667		mov r8, r7
668		mov r6, r8
669		@ Stack pointer unchanged, no stack allocated variables
670	.L8:
671		cmp r6, #0
672		bne .L9
673		mov r10, r5
674		mov r8, #0
675		mov r3, r0
676		bl _arrStore4
677		adds r8, r5, #1
678		blvs _errOverflow
679		push {r8}
680		pop {r8}
681		mov r8, r8
682		mov r5, r8
683		@ Stack pointer unchanged, no stack allocated variables
684	.L6:
685		cmp r5, r4
686		blt .L7
687		mov r0, #1
688		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
689		mov sp, fp
690		pop {r4, r5, r6, r7}
691		pop {fp, pc}
692		@ Stack pointer unchanged, no stack allocated variables
693		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
694		@ the .ltorg here is used to generate the constant pool after every function
695		@ this is important for ensuring string literals and large integers are accessible!
696		.ltorg
697	
698	wacc_count:
699		push {fp, lr}
700		push {r4, r5, r6, r7}
701		mov fp, sp
702		@ Stack pointer unchanged, no stack allocated variables
703		ldr r8, [r0, #-4]
704		mov r8, r8
705		mov r4, r8
706		mov r8, #0
707		mov r5, r8
708		mov r8, #0
709		mov r6, r8
710		b .L10
711	.L11:
712		@ Stack pointer unchanged, no stack allocated variables
713		push {r0}
714		@ Set up R12 as a temporary second base pointer for the caller saved things
715		mov r12, sp
716		@ Stack pointer unchanged, no stack allocated arguments
717		mov r10, r6
718		mov r3, r0
719		bl _arrLoad4
720		mov r8, r3
721		mov r8, r8
722		mov r0, r8
723		bl wacc_countNodes
724		mov r12, r0
725		@ Stack pointer unchanged, no stack allocated arguments
726		pop {r0}
727		mov r8, r12
728		mov r7, r8
729		adds r8, r5, r7
730		blvs _errOverflow
731		push {r8}
732		pop {r8}
733		mov r8, r8
734		mov r5, r8
735		adds r8, r6, #1
736		blvs _errOverflow
737		push {r8}
738		pop {r8}
739		mov r8, r8
740		mov r6, r8
741		@ Stack pointer unchanged, no stack allocated variables
742	.L10:
743		cmp r6, r4
744		blt .L11
745		mov r0, r5
746		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
747		mov sp, fp
748		pop {r4, r5, r6, r7}
749		pop {fp, pc}
750		@ Stack pointer unchanged, no stack allocated variables
751		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
752		@ the .ltorg here is used to generate the constant pool after every function
753		@ this is important for ensuring string literals and large integers are accessible!
754		.ltorg
755	
756	wacc_printAll:
757		push {fp, lr}
758		push {r4, r5, r6}
759		mov fp, sp
760		@ Stack pointer unchanged, no stack allocated variables
761		ldr r8, [r0, #-4]
762		mov r8, r8
763		mov r4, r8
764		mov r8, #0
765		mov r5, r8
766		b .L12
767	.L13:
768		@ Stack pointer unchanged, no stack allocated variables
769		push {r0}
770		@ Set up R12 as a temporary second base pointer for the caller saved things
771		mov r12, sp
772		@ Stack pointer unchanged, no stack allocated arguments
773		mov r10, r5
774		mov r3, r0
775		bl _arrLoad4
776		mov r8, r3
777		mov r8, r8
778		mov r0, r8
779		bl wacc_printAllNodes
780		mov r12, r0
781		@ Stack pointer unchanged, no stack allocated arguments
782		pop {r0}
783		mov r8, r12
784		mov r6, r8
785		adds r8, r5, #1
786		blvs _errOverflow
787		push {r8}
788		pop {r8}
789		mov r8, r8
790		mov r5, r8
791		@ Stack pointer unchanged, no stack allocated variables
792	.L12:
793		cmp r5, r4
794		blt .L13
795		push {r0}
796		@ Set up R12 as a temporary second base pointer for the caller saved things
797		mov r12, sp
798		@ Stack pointer unchanged, no stack allocated arguments
799		ldr r8, =.L.str0
800		push {r8}
801		pop {r8}
802		mov r8, r8
803		mov r0, r8
804		@ statement primitives do not return results (but will clobber r0/rax)
805		bl _prints
806		bl _println
807		pop {r0}
808		mov r0, #1
809		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
810		mov sp, fp
811		pop {r4, r5, r6}
812		pop {fp, pc}
813		@ Stack pointer unchanged, no stack allocated variables
814		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
815		@ the .ltorg here is used to generate the constant pool after every function
816		@ this is important for ensuring string literals and large integers are accessible!
817		.ltorg
818	
819	wacc_calculateIndex:
820		push {fp, lr}
821		push {r4}
822		mov fp, sp
823		@ Stack pointer unchanged, no stack allocated variables
824		ldr r8, [r0, #-4]
825		mov r8, r8
826		mov r4, r8
827		push {r0, r1}
828		mov r0, r1
829		mov r1, r4
830		cmp r1, #0
831		bleq _errDivZero
832		bl __aeabi_idivmod
833		mov r12, r1
834		pop {r0, r1}
835		mov r8, r12
836		push {r8}
837		pop {r0}
838		mov r0, r0
839		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
840		mov sp, fp
841		pop {r4}
842		pop {fp, pc}
843		@ Stack pointer unchanged, no stack allocated variables
844		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
845		@ the .ltorg here is used to generate the constant pool after every function
846		@ this is important for ensuring string literals and large integers are accessible!
847		.ltorg
848	
849	wacc_findNode:
850		push {fp, lr}
851		push {r4}
852		mov fp, sp
853		b .L14
854	.L15:
855		@ Stack pointer unchanged, no stack allocated variables
856		cmp r0, #0
857		bleq _errNull
858		ldr r8, [r0, #0]
859		mov r8, r8
860		mov r4, r8
861		cmp r4, r1
862		beq .L16
863		cmp r0, #0
864		bleq _errNull
865		ldr r8, [r0, #4]
866		mov r8, r8
867		push {r8}
868		pop {r8}
869		mov r8, r8
870		mov r0, r8
871		b .L17
872	.L16:
873		mov r0, r0
874		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
875		mov sp, fp
876		pop {r4}
877		pop {fp, pc}
878	.L17:
879		@ Stack pointer unchanged, no stack allocated variables
880	.L14:
881		cmp r0, #0
882		bne .L15
883		mov r0, #0
884		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
885		mov sp, fp
886		pop {r4}
887		pop {fp, pc}
888		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
889		@ the .ltorg here is used to generate the constant pool after every function
890		@ this is important for ensuring string literals and large integers are accessible!
891		.ltorg
892	
893	wacc_removeNode:
894		push {fp, lr}
895		push {r4}
896		mov fp, sp
897		cmp r0, #0
898		beq .L18
899		cmp r0, r1
900		beq .L20
901		@ Stack pointer unchanged, no stack allocated variables
902		cmp r0, #0
903		bleq _errNull
904		ldr r8, [r0, #4]
905		mov r8, r8
906		mov r4, r8
907		push {r0, r1}
908		@ Set up R12 as a temporary second base pointer for the caller saved things
909		mov r12, sp
910		@ Stack pointer unchanged, no stack allocated arguments
911		mov r8, r4
912		mov r0, r8
913		mov r8, r1
914		mov r1, r8
915		bl wacc_removeNode
916		mov r12, r0
917		@ Stack pointer unchanged, no stack allocated arguments
918		pop {r0, r1}
919		cmp r0, #0
920		bleq _errNull
921		mov r8, r12
922		str r8, [r0, #4]
923		mov r0, r0
924		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
925		mov sp, fp
926		pop {r4}
927		pop {fp, pc}
928		@ Stack pointer unchanged, no stack allocated variables
929		b .L21
930	.L20:
931		cmp r0, #0
932		bleq _errNull
933		ldr r8, [r0, #4]
934		mov r8, r8
935		push {r8}
936		pop {r8}
937		mov r8, r8
938		mov r0, r8
939		push {r0, r1}
940		@ Set up R12 as a temporary second base pointer for the caller saved things
941		mov r12, sp
942		@ Stack pointer unchanged, no stack allocated arguments
943		mov r8, r1
944		mov r0, r8
945		@ statement primitives do not return results (but will clobber r0/rax)
946		bl _freepair
947		pop {r0, r1}
948		mov r0, r0
949		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
950		mov sp, fp
951		pop {r4}
952		pop {fp, pc}
953	.L21:
954		b .L19
955	.L18:
956		mov r0, #0
957		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
958		mov sp, fp
959		pop {r4}
960		pop {fp, pc}
961	.L19:
962		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
963		@ the .ltorg here is used to generate the constant pool after every function
964		@ this is important for ensuring string literals and large integers are accessible!
965		.ltorg
966	
967	wacc_countNodes:
968		push {fp, lr}
969		push {r4}
970		mov fp, sp
971		@ Stack pointer unchanged, no stack allocated variables
972		mov r8, #0
973		mov r4, r8
974		b .L22
975	.L23:
976		adds r8, r4, #1
977		blvs _errOverflow
978		push {r8}
979		pop {r8}
980		mov r8, r8
981		mov r4, r8
982		cmp r0, #0
983		bleq _errNull
984		ldr r8, [r0, #4]
985		mov r8, r8
986		push {r8}
987		pop {r8}
988		mov r8, r8
989		mov r0, r8
990	.L22:
991		cmp r0, #0
992		bne .L23
993		mov r0, r4
994		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
995		mov sp, fp
996		pop {r4}
997		pop {fp, pc}
998		@ Stack pointer unchanged, no stack allocated variables
999		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1000		@ the .ltorg here is used to generate the constant pool after every function
1001		@ this is important for ensuring string literals and large integers are accessible!
1002		.ltorg
1003	
1004	wacc_printAllNodes:
1005		push {fp, lr}
1006		push {r4}
1007		mov fp, sp
1008		b .L24
1009	.L25:
1010		@ Stack pointer unchanged, no stack allocated variables
1011		cmp r0, #0
1012		bleq _errNull
1013		ldr r8, [r0, #0]
1014		mov r8, r8
1015		mov r4, r8
1016		push {r0}
1017		@ Set up R12 as a temporary second base pointer for the caller saved things
1018		mov r12, sp
1019		@ Stack pointer unchanged, no stack allocated arguments
1020		mov r8, r4
1021		mov r0, r8
1022		@ statement primitives do not return results (but will clobber r0/rax)
1023		bl _printi
1024		pop {r0}
1025		push {r0}
1026		@ Set up R12 as a temporary second base pointer for the caller saved things
1027		mov r12, sp
1028		@ Stack pointer unchanged, no stack allocated arguments
1029		mov r8, #32
1030		mov r0, r8
1031		@ statement primitives do not return results (but will clobber r0/rax)
1032		bl _printc
1033		pop {r0}
1034		cmp r0, #0
1035		bleq _errNull
1036		ldr r8, [r0, #4]
1037		mov r8, r8
1038		push {r8}
1039		pop {r8}
1040		mov r8, r8
1041		mov r0, r8
1042		@ Stack pointer unchanged, no stack allocated variables
1043	.L24:
1044		cmp r0, #0
1045		bne .L25
1046		mov r0, #1
1047		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1048		mov sp, fp
1049		pop {r4}
1050		pop {fp, pc}
1051		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1052		@ the .ltorg here is used to generate the constant pool after every function
1053		@ this is important for ensuring string literals and large integers are accessible!
1054		.ltorg
1055	
1056	wacc_printMenu:
1057		push {fp, lr}
1058		push {r4, r5, r6, r7}
1059		mov fp, sp
1060		@ Stack pointer unchanged, no stack allocated variables
1061		@ Stack pointer unchanged, no stack allocated arguments
1062		ldr r8, =.L.str1
1063		push {r8}
1064		pop {r8}
1065		mov r8, r8
1066		mov r0, r8
1067		@ statement primitives do not return results (but will clobber r0/rax)
1068		bl _prints
1069		bl _println
1070		@ Stack pointer unchanged, no stack allocated arguments
1071		ldr r8, =.L.str2
1072		push {r8}
1073		pop {r8}
1074		mov r8, r8
1075		mov r0, r8
1076		@ statement primitives do not return results (but will clobber r0/rax)
1077		bl _prints
1078		bl _println
1079		@ Stack pointer unchanged, no stack allocated arguments
1080		ldr r8, =.L.str1
1081		push {r8}
1082		pop {r8}
1083		mov r8, r8
1084		mov r0, r8
1085		@ statement primitives do not return results (but will clobber r0/rax)
1086		bl _prints
1087		bl _println
1088		@ Stack pointer unchanged, no stack allocated arguments
1089		ldr r8, =.L.str3
1090		push {r8}
1091		pop {r8}
1092		mov r8, r8
1093		mov r0, r8
1094		@ statement primitives do not return results (but will clobber r0/rax)
1095		bl _prints
1096		bl _println
1097		@ Stack pointer unchanged, no stack allocated arguments
1098		ldr r8, =.L.str4
1099		push {r8}
1100		pop {r8}
1101		mov r8, r8
1102		mov r0, r8
1103		@ statement primitives do not return results (but will clobber r0/rax)
1104		bl _prints
1105		bl _println
1106		@ Stack pointer unchanged, no stack allocated arguments
1107		ldr r8, =.L.str3
1108		push {r8}
1109		pop {r8}
1110		mov r8, r8
1111		mov r0, r8
1112		@ statement primitives do not return results (but will clobber r0/rax)
1113		bl _prints
1114		bl _println
1115		@ Stack pointer unchanged, no stack allocated arguments
1116		ldr r8, =.L.str5
1117		push {r8}
1118		pop {r8}
1119		mov r8, r8
1120		mov r0, r8
1121		@ statement primitives do not return results (but will clobber r0/rax)
1122		bl _prints
1123		bl _println
1124		@ Stack pointer unchanged, no stack allocated arguments
1125		ldr r8, =.L.str6
1126		push {r8}
1127		pop {r8}
1128		mov r8, r8
1129		mov r0, r8
1130		@ statement primitives do not return results (but will clobber r0/rax)
1131		bl _prints
1132		bl _println
1133		@ Stack pointer unchanged, no stack allocated arguments
1134		ldr r8, =.L.str7
1135		push {r8}
1136		pop {r8}
1137		mov r8, r8
1138		mov r0, r8
1139		@ statement primitives do not return results (but will clobber r0/rax)
1140		bl _prints
1141		bl _println
1142		@ Stack pointer unchanged, no stack allocated arguments
1143		ldr r8, =.L.str8
1144		push {r8}
1145		pop {r8}
1146		mov r8, r8
1147		mov r0, r8
1148		@ statement primitives do not return results (but will clobber r0/rax)
1149		bl _prints
1150		bl _println
1151		@ Stack pointer unchanged, no stack allocated arguments
1152		ldr r8, =.L.str9
1153		push {r8}
1154		pop {r8}
1155		mov r8, r8
1156		mov r0, r8
1157		@ statement primitives do not return results (but will clobber r0/rax)
1158		bl _prints
1159		bl _println
1160		@ Stack pointer unchanged, no stack allocated arguments
1161		ldr r8, =.L.str10
1162		push {r8}
1163		pop {r8}
1164		mov r8, r8
1165		mov r0, r8
1166		@ statement primitives do not return results (but will clobber r0/rax)
1167		bl _prints
1168		bl _println
1169		@ Stack pointer unchanged, no stack allocated arguments
1170		ldr r8, =.L.str11
1171		push {r8}
1172		pop {r8}
1173		mov r8, r8
1174		mov r0, r8
1175		@ statement primitives do not return results (but will clobber r0/rax)
1176		bl _prints
1177		bl _println
1178		@ Stack pointer unchanged, no stack allocated arguments
1179		ldr r8, =.L.str3
1180		push {r8}
1181		pop {r8}
1182		mov r8, r8
1183		mov r0, r8
1184		@ statement primitives do not return results (but will clobber r0/rax)
1185		bl _prints
1186		bl _println
1187		@ Stack pointer unchanged, no stack allocated arguments
1188		ldr r8, =.L.str1
1189		push {r8}
1190		pop {r8}
1191		mov r8, r8
1192		mov r0, r8
1193		@ statement primitives do not return results (but will clobber r0/rax)
1194		bl _prints
1195		bl _println
1196		mov r8, #97
1197		mov r4, r8
1198		mov r8, #103
1199		mov r5, r8
1200		b .L26
1201	.L27:
1202		@ Stack pointer unchanged, no stack allocated variables
1203		@ Stack pointer unchanged, no stack allocated arguments
1204		ldr r8, =.L.str12
1205		push {r8}
1206		pop {r8}
1207		mov r8, r8
1208		mov r0, r8
1209		@ statement primitives do not return results (but will clobber r0/rax)
1210		bl _prints
1211		mov r8, #0
1212		mov r6, r8
1213		@ Stack pointer unchanged, no stack allocated arguments
1214		@ load the current value in the destination of the read so it supports defaults
1215		mov r8, r6
1216		mov r0, r8
1217		bl _readc
1218		mov r12, r0
1219		mov r8, r12
1220		mov r6, r8
1221		mov r8, r6
1222		mov r7, r8
1223		cmp r4, r7
1224		movle r8, #1
1225		movgt r8, #0
1226		push {r8}
1227		pop {r8}
1228		cmp r8, #1
1229		bne .L30
1230		cmp r7, r5
1231		movle r8, #1
1232		movgt r8, #0
1233		push {r8}
1234		pop {r8}
1235		cmp r8, #1
1236	.L30:
1237		beq .L28
1238		@ Stack pointer unchanged, no stack allocated arguments
1239		ldr r8, =.L.str13
1240		push {r8}
1241		pop {r8}
1242		mov r8, r8
1243		mov r0, r8
1244		@ statement primitives do not return results (but will clobber r0/rax)
1245		bl _prints
1246		@ Stack pointer unchanged, no stack allocated arguments
1247		mov r8, r6
1248		mov r0, r8
1249		@ statement primitives do not return results (but will clobber r0/rax)
1250		bl _printc
1251		@ Stack pointer unchanged, no stack allocated arguments
1252		ldr r8, =.L.str14
1253		push {r8}
1254		pop {r8}
1255		mov r8, r8
1256		mov r0, r8
1257		@ statement primitives do not return results (but will clobber r0/rax)
1258		bl _prints
1259		bl _println
1260		b .L29
1261	.L28:
1262		mov r0, r6
1263		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1264		mov sp, fp
1265		pop {r4, r5, r6, r7}
1266		pop {fp, pc}
1267	.L29:
1268		@ Stack pointer unchanged, no stack allocated variables
1269	.L26:
1270		mov r8, #1
1271		cmp r8, #1
1272		beq .L27
1273		mov r0, #0
1274		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1275		mov sp, fp
1276		pop {r4, r5, r6, r7}
1277		pop {fp, pc}
1278		@ Stack pointer unchanged, no stack allocated variables
1279		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1280		@ the .ltorg here is used to generate the constant pool after every function
1281		@ this is important for ensuring string literals and large integers are accessible!
1282		.ltorg
1283	
1284	wacc_askForInt:
1285		push {fp, lr}
1286		push {r4}
1287		mov fp, sp
1288		@ Stack pointer unchanged, no stack allocated variables
1289		push {r0}
1290		@ Set up R12 as a temporary second base pointer for the caller saved things
1291		mov r12, sp
1292		@ Stack pointer unchanged, no stack allocated arguments
1293		mov r8, r0
1294		mov r0, r8
1295		@ statement primitives do not return results (but will clobber r0/rax)
1296		bl _prints
1297		pop {r0}
1298		mov r8, #0
1299		mov r4, r8
1300		push {r0}
1301		@ Set up R12 as a temporary second base pointer for the caller saved things
1302		mov r12, sp
1303		@ Stack pointer unchanged, no stack allocated arguments
1304		@ load the current value in the destination of the read so it supports defaults
1305		mov r8, r4
1306		mov r0, r8
1307		bl _readi
1308		mov r12, r0
1309		pop {r0}
1310		mov r8, r12
1311		mov r4, r8
1312		push {r0}
1313		@ Set up R12 as a temporary second base pointer for the caller saved things
1314		mov r12, sp
1315		@ Stack pointer unchanged, no stack allocated arguments
1316		ldr r8, =.L.str13
1317		push {r8}
1318		pop {r8}
1319		mov r8, r8
1320		mov r0, r8
1321		@ statement primitives do not return results (but will clobber r0/rax)
1322		bl _prints
1323		pop {r0}
1324		push {r0}
1325		@ Set up R12 as a temporary second base pointer for the caller saved things
1326		mov r12, sp
1327		@ Stack pointer unchanged, no stack allocated arguments
1328		mov r8, r4
1329		mov r0, r8
1330		@ statement primitives do not return results (but will clobber r0/rax)
1331		bl _printi
1332		bl _println
1333		pop {r0}
1334		mov r0, r4
1335		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1336		mov sp, fp
1337		pop {r4}
1338		pop {fp, pc}
1339		@ Stack pointer unchanged, no stack allocated variables
1340		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1341		@ the .ltorg here is used to generate the constant pool after every function
1342		@ this is important for ensuring string literals and large integers are accessible!
1343		.ltorg
1344	
1345	wacc_handleMenuInsert:
1346		push {fp, lr}
1347		push {r4, r5}
1348		mov fp, sp
1349		@ Stack pointer unchanged, no stack allocated variables
1350		push {r0}
1351		@ Set up R12 as a temporary second base pointer for the caller saved things
1352		mov r12, sp
1353		@ Stack pointer unchanged, no stack allocated arguments
1354		ldr r8, =.L.str15
1355		push {r8}
1356		pop {r8}
1357		mov r8, r8
1358		mov r0, r8
1359		bl wacc_askForInt
1360		mov r12, r0
1361		@ Stack pointer unchanged, no stack allocated arguments
1362		pop {r0}
1363		mov r8, r12
1364		mov r4, r8
1365		push {r0}
1366		@ Set up R12 as a temporary second base pointer for the caller saved things
1367		mov r12, sp
1368		@ Stack pointer unchanged, no stack allocated arguments
1369		mov r8, r0
1370		mov r0, r8
1371		mov r8, r4
1372		mov r1, r8
1373		bl wacc_insertIfNotContain
1374		mov r12, r0
1375		@ Stack pointer unchanged, no stack allocated arguments
1376		pop {r0}
1377		mov r8, r12
1378		mov r5, r8
1379		cmp r5, #1
1380		beq .L31
1381		push {r0}
1382		@ Set up R12 as a temporary second base pointer for the caller saved things
1383		mov r12, sp
1384		@ Stack pointer unchanged, no stack allocated arguments
1385		ldr r8, =.L.str16
1386		push {r8}
1387		pop {r8}
1388		mov r8, r8
1389		mov r0, r8
1390		@ statement primitives do not return results (but will clobber r0/rax)
1391		bl _prints
1392		bl _println
1393		pop {r0}
1394		b .L32
1395	.L31:
1396		push {r0}
1397		@ Set up R12 as a temporary second base pointer for the caller saved things
1398		mov r12, sp
1399		@ Stack pointer unchanged, no stack allocated arguments
1400		ldr r8, =.L.str17
1401		push {r8}
1402		pop {r8}
1403		mov r8, r8
1404		mov r0, r8
1405		@ statement primitives do not return results (but will clobber r0/rax)
1406		bl _prints
1407		bl _println
1408		pop {r0}
1409	.L32:
1410		mov r0, #1
1411		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1412		mov sp, fp
1413		pop {r4, r5}
1414		pop {fp, pc}
1415		@ Stack pointer unchanged, no stack allocated variables
1416		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1417		@ the .ltorg here is used to generate the constant pool after every function
1418		@ this is important for ensuring string literals and large integers are accessible!
1419		.ltorg
1420	
1421	wacc_handleMenuFind:
1422		push {fp, lr}
1423		push {r4, r5}
1424		mov fp, sp
1425		@ Stack pointer unchanged, no stack allocated variables
1426		push {r0}
1427		@ Set up R12 as a temporary second base pointer for the caller saved things
1428		mov r12, sp
1429		@ Stack pointer unchanged, no stack allocated arguments
1430		ldr r8, =.L.str18
1431		push {r8}
1432		pop {r8}
1433		mov r8, r8
1434		mov r0, r8
1435		bl wacc_askForInt
1436		mov r12, r0
1437		@ Stack pointer unchanged, no stack allocated arguments
1438		pop {r0}
1439		mov r8, r12
1440		mov r4, r8
1441		push {r0}
1442		@ Set up R12 as a temporary second base pointer for the caller saved things
1443		mov r12, sp
1444		@ Stack pointer unchanged, no stack allocated arguments
1445		mov r8, r0
1446		mov r0, r8
1447		mov r8, r4
1448		mov r1, r8
1449		bl wacc_contain
1450		mov r12, r0
1451		@ Stack pointer unchanged, no stack allocated arguments
1452		pop {r0}
1453		mov r8, r12
1454		mov r5, r8
1455		cmp r5, #1
1456		beq .L33
1457		push {r0}
1458		@ Set up R12 as a temporary second base pointer for the caller saved things
1459		mov r12, sp
1460		@ Stack pointer unchanged, no stack allocated arguments
1461		ldr r8, =.L.str19
1462		push {r8}
1463		pop {r8}
1464		mov r8, r8
1465		mov r0, r8
1466		@ statement primitives do not return results (but will clobber r0/rax)
1467		bl _prints
1468		bl _println
1469		pop {r0}
1470		b .L34
1471	.L33:
1472		push {r0}
1473		@ Set up R12 as a temporary second base pointer for the caller saved things
1474		mov r12, sp
1475		@ Stack pointer unchanged, no stack allocated arguments
1476		ldr r8, =.L.str20
1477		push {r8}
1478		pop {r8}
1479		mov r8, r8
1480		mov r0, r8
1481		@ statement primitives do not return results (but will clobber r0/rax)
1482		bl _prints
1483		bl _println
1484		pop {r0}
1485	.L34:
1486		mov r0, #1
1487		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1488		mov sp, fp
1489		pop {r4, r5}
1490		pop {fp, pc}
1491		@ Stack pointer unchanged, no stack allocated variables
1492		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1493		@ the .ltorg here is used to generate the constant pool after every function
1494		@ this is important for ensuring string literals and large integers are accessible!
1495		.ltorg
1496	
1497	wacc_handleMenuCount:
1498		push {fp, lr}
1499		push {r4}
1500		mov fp, sp
1501		@ Stack pointer unchanged, no stack allocated variables
1502		push {r0}
1503		@ Set up R12 as a temporary second base pointer for the caller saved things
1504		mov r12, sp
1505		@ Stack pointer unchanged, no stack allocated arguments
1506		mov r8, r0
1507		mov r0, r8
1508		bl wacc_count
1509		mov r12, r0
1510		@ Stack pointer unchanged, no stack allocated arguments
1511		pop {r0}
1512		mov r8, r12
1513		mov r4, r8
1514		cmp r4, #1
1515		beq .L35
1516		push {r0}
1517		@ Set up R12 as a temporary second base pointer for the caller saved things
1518		mov r12, sp
1519		@ Stack pointer unchanged, no stack allocated arguments
1520		ldr r8, =.L.str21
1521		push {r8}
1522		pop {r8}
1523		mov r8, r8
1524		mov r0, r8
1525		@ statement primitives do not return results (but will clobber r0/rax)
1526		bl _prints
1527		pop {r0}
1528		push {r0}
1529		@ Set up R12 as a temporary second base pointer for the caller saved things
1530		mov r12, sp
1531		@ Stack pointer unchanged, no stack allocated arguments
1532		mov r8, r4
1533		mov r0, r8
1534		@ statement primitives do not return results (but will clobber r0/rax)
1535		bl _printi
1536		pop {r0}
1537		push {r0}
1538		@ Set up R12 as a temporary second base pointer for the caller saved things
1539		mov r12, sp
1540		@ Stack pointer unchanged, no stack allocated arguments
1541		ldr r8, =.L.str22
1542		push {r8}
1543		pop {r8}
1544		mov r8, r8
1545		mov r0, r8
1546		@ statement primitives do not return results (but will clobber r0/rax)
1547		bl _prints
1548		bl _println
1549		pop {r0}
1550		b .L36
1551	.L35:
1552		push {r0}
1553		@ Set up R12 as a temporary second base pointer for the caller saved things
1554		mov r12, sp
1555		@ Stack pointer unchanged, no stack allocated arguments
1556		ldr r8, =.L.str23
1557		push {r8}
1558		pop {r8}
1559		mov r8, r8
1560		mov r0, r8
1561		@ statement primitives do not return results (but will clobber r0/rax)
1562		bl _prints
1563		bl _println
1564		pop {r0}
1565	.L36:
1566		mov r0, #1
1567		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1568		mov sp, fp
1569		pop {r4}
1570		pop {fp, pc}
1571		@ Stack pointer unchanged, no stack allocated variables
1572		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1573		@ the .ltorg here is used to generate the constant pool after every function
1574		@ this is important for ensuring string literals and large integers are accessible!
1575		.ltorg
1576	
1577	wacc_handleMenuPrint:
1578		push {fp, lr}
1579		push {r4}
1580		mov fp, sp
1581		@ Stack pointer unchanged, no stack allocated variables
1582		push {r0}
1583		@ Set up R12 as a temporary second base pointer for the caller saved things
1584		mov r12, sp
1585		@ Stack pointer unchanged, no stack allocated arguments
1586		ldr r8, =.L.str24
1587		push {r8}
1588		pop {r8}
1589		mov r8, r8
1590		mov r0, r8
1591		@ statement primitives do not return results (but will clobber r0/rax)
1592		bl _prints
1593		pop {r0}
1594		push {r0}
1595		@ Set up R12 as a temporary second base pointer for the caller saved things
1596		mov r12, sp
1597		@ Stack pointer unchanged, no stack allocated arguments
1598		mov r8, r0
1599		mov r0, r8
1600		bl wacc_printAll
1601		mov r12, r0
1602		@ Stack pointer unchanged, no stack allocated arguments
1603		pop {r0}
1604		mov r8, r12
1605		mov r4, r8
1606		mov r0, #1
1607		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1608		mov sp, fp
1609		pop {r4}
1610		pop {fp, pc}
1611		@ Stack pointer unchanged, no stack allocated variables
1612		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1613		@ the .ltorg here is used to generate the constant pool after every function
1614		@ this is important for ensuring string literals and large integers are accessible!
1615		.ltorg
1616	
1617	wacc_handleMenuRemove:
1618		push {fp, lr}
1619		push {r4, r5}
1620		mov fp, sp
1621		@ Stack pointer unchanged, no stack allocated variables
1622		push {r0}
1623		@ Set up R12 as a temporary second base pointer for the caller saved things
1624		mov r12, sp
1625		@ Stack pointer unchanged, no stack allocated arguments
1626		ldr r8, =.L.str25
1627		push {r8}
1628		pop {r8}
1629		mov r8, r8
1630		mov r0, r8
1631		bl wacc_askForInt
1632		mov r12, r0
1633		@ Stack pointer unchanged, no stack allocated arguments
1634		pop {r0}
1635		mov r8, r12
1636		mov r4, r8
1637		push {r0}
1638		@ Set up R12 as a temporary second base pointer for the caller saved things
1639		mov r12, sp
1640		@ Stack pointer unchanged, no stack allocated arguments
1641		mov r8, r0
1642		mov r0, r8
1643		mov r8, r4
1644		mov r1, r8
1645		bl wacc_remove
1646		mov r12, r0
1647		@ Stack pointer unchanged, no stack allocated arguments
1648		pop {r0}
1649		mov r8, r12
1650		mov r5, r8
1651		cmp r5, #1
1652		beq .L37
1653		push {r0}
1654		@ Set up R12 as a temporary second base pointer for the caller saved things
1655		mov r12, sp
1656		@ Stack pointer unchanged, no stack allocated arguments
1657		ldr r8, =.L.str19
1658		push {r8}
1659		pop {r8}
1660		mov r8, r8
1661		mov r0, r8
1662		@ statement primitives do not return results (but will clobber r0/rax)
1663		bl _prints
1664		bl _println
1665		pop {r0}
1666		b .L38
1667	.L37:
1668		push {r0}
1669		@ Set up R12 as a temporary second base pointer for the caller saved things
1670		mov r12, sp
1671		@ Stack pointer unchanged, no stack allocated arguments
1672		ldr r8, =.L.str26
1673		push {r8}
1674		pop {r8}
1675		mov r8, r8
1676		mov r0, r8
1677		@ statement primitives do not return results (but will clobber r0/rax)
1678		bl _prints
1679		bl _println
1680		pop {r0}
1681	.L38:
1682		mov r0, #1
1683		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1684		mov sp, fp
1685		pop {r4, r5}
1686		pop {fp, pc}
1687		@ Stack pointer unchanged, no stack allocated variables
1688		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1689		@ the .ltorg here is used to generate the constant pool after every function
1690		@ this is important for ensuring string literals and large integers are accessible!
1691		.ltorg
1692	
1693	wacc_handleMenuRemoveAll:
1694		push {fp, lr}
1695		push {r4}
1696		mov fp, sp
1697		@ Stack pointer unchanged, no stack allocated variables
1698		push {r0}
1699		@ Set up R12 as a temporary second base pointer for the caller saved things
1700		mov r12, sp
1701		@ Stack pointer unchanged, no stack allocated arguments
1702		mov r8, r0
1703		mov r0, r8
1704		bl wacc_removeAll
1705		mov r12, r0
1706		@ Stack pointer unchanged, no stack allocated arguments
1707		pop {r0}
1708		mov r8, r12
1709		mov r4, r8
1710		push {r0}
1711		@ Set up R12 as a temporary second base pointer for the caller saved things
1712		mov r12, sp
1713		@ Stack pointer unchanged, no stack allocated arguments
1714		ldr r8, =.L.str27
1715		push {r8}
1716		pop {r8}
1717		mov r8, r8
1718		mov r0, r8
1719		@ statement primitives do not return results (but will clobber r0/rax)
1720		bl _prints
1721		bl _println
1722		pop {r0}
1723		mov r0, #1
1724		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1725		mov sp, fp
1726		pop {r4}
1727		pop {fp, pc}
1728		@ Stack pointer unchanged, no stack allocated variables
1729		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
1730		@ the .ltorg here is used to generate the constant pool after every function
1731		@ this is important for ensuring string literals and large integers are accessible!
1732		.ltorg
1733	
1734	_freepair:
1735		push {fp, lr}
1736		mov fp, sp
1737		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1738		bic sp, sp, #0x7
1739		cmp r0, #0
1740		bleq _errNull
1741		bl free
1742		mov sp, fp
1743		pop {fp, pc}
1744	
1745	@ length of .L._readc_str0
1746		.word 3
1747	.L._readc_str0:
1748		.asciz " %c"
1749	.align 4
1750	_readc:
1751		push {fp, lr}
1752		mov fp, sp
1753		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1754		bic sp, sp, #0x7
1755		@ R0 contains the "original" value of the destination of the read
1756		@ allocate space on the stack to store the read: preserve alignment!
1757		@ the passed default argument should be stored in case of EOF
1758		sub sp, sp, #8
1759		strb r0, [sp, #0]
1760		mov r1, sp
1761		adr r0, .L._readc_str0
1762		bl scanf
1763		ldrsb r0, [sp, #0]
1764		add sp, sp, #8
1765		mov sp, fp
1766		pop {fp, pc}
1767	
1768	@ length of .L._prints_str0
1769		.word 4
1770	.L._prints_str0:
1771		.asciz "%.*s"
1772	.align 4
1773	_prints:
1774		push {fp, lr}
1775		mov fp, sp
1776		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1777		bic sp, sp, #0x7
1778		mov r2, r0
1779		ldr r1, [r0, #-4]
1780		adr r0, .L._prints_str0
1781		bl printf
1782		mov r0, #0
1783		bl fflush
1784		mov sp, fp
1785		pop {fp, pc}
1786	
1787	@ length of .L._readi_str0
1788		.word 2
1789	.L._readi_str0:
1790		.asciz "%d"
1791	.align 4
1792	_readi:
1793		push {fp, lr}
1794		mov fp, sp
1795		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1796		bic sp, sp, #0x7
1797		@ R0 contains the "original" value of the destination of the read
1798		@ allocate space on the stack to store the read: preserve alignment!
1799		@ the passed default argument should be stored in case of EOF
1800		sub sp, sp, #8
1801		str r0, [sp, #0]
1802		mov r1, sp
1803		adr r0, .L._readi_str0
1804		bl scanf
1805		ldr r0, [sp, #0]
1806		add sp, sp, #8
1807		mov sp, fp
1808		pop {fp, pc}
1809	
1810	_malloc:
1811		push {fp, lr}
1812		mov fp, sp
1813		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1814		bic sp, sp, #0x7
1815		bl malloc
1816		cmp r0, #0
1817		bleq _errOutOfMemory
1818		mov sp, fp
1819		pop {fp, pc}
1820	
1821	@ length of .L._printi_str0
1822		.word 2
1823	.L._printi_str0:
1824		.asciz "%d"
1825	.align 4
1826	_printi:
1827		push {fp, lr}
1828		mov fp, sp
1829		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1830		bic sp, sp, #0x7
1831		mov r1, r0
1832		adr r0, .L._printi_str0
1833		bl printf
1834		mov r0, #0
1835		bl fflush
1836		mov sp, fp
1837		pop {fp, pc}
1838	
1839	@ length of .L._printc_str0
1840		.word 2
1841	.L._printc_str0:
1842		.asciz "%c"
1843	.align 4
1844	_printc:
1845		push {fp, lr}
1846		mov fp, sp
1847		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1848		bic sp, sp, #0x7
1849		mov r1, r0
1850		adr r0, .L._printc_str0
1851		bl printf
1852		mov r0, #0
1853		bl fflush
1854		mov sp, fp
1855		pop {fp, pc}
1856	
1857	@ length of .L._println_str0
1858		.word 0
1859	.L._println_str0:
1860		.asciz ""
1861	.align 4
1862	_println:
1863		push {fp, lr}
1864		mov fp, sp
1865		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1866		bic sp, sp, #0x7
1867		adr r0, .L._println_str0
1868		bl puts
1869		mov r0, #0
1870		bl fflush
1871		mov sp, fp
1872		pop {fp, pc}
1873	
1874	_exit:
1875		push {fp, lr}
1876		mov fp, sp
1877		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1878		bic sp, sp, #0x7
1879		bl exit
1880		mov sp, fp
1881		pop {fp, pc}
1882	
1883	_arrStore4:
1884		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
1885		push {lr}
1886		cmp r10, #0
1887		movlt r1, r10
1888		bllt _errOutOfBounds
1889		ldr lr, [r3, #-4]
1890		cmp r10, lr
1891		movge r1, r10
1892		blge _errOutOfBounds
1893		str r8, [r3, r10, lsl #2]
1894		pop {pc}
1895	
1896	_arrLoad4:
1897		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
1898		push {lr}
1899		cmp r10, #0
1900		movlt r1, r10
1901		bllt _errOutOfBounds
1902		ldr lr, [r3, #-4]
1903		cmp r10, lr
1904		movge r1, r10
1905		blge _errOutOfBounds
1906		ldr r3, [r3, r10, lsl #2]
1907		pop {pc}
1908	
1909	@ length of .L._errOutOfMemory_str0
1910		.word 27
1911	.L._errOutOfMemory_str0:
1912		.asciz "fatal error: out of memory\n"
1913	.align 4
1914	_errOutOfMemory:
1915		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1916		bic sp, sp, #0x7
1917		adr r0, .L._errOutOfMemory_str0
1918		bl _prints
1919		mov r0, #255
1920		bl _exit
1921	
1922	@ length of .L._errDivZero_str0
1923		.word 40
1924	.L._errDivZero_str0:
1925		.asciz "fatal error: division or modulo by zero\n"
1926	.align 4
1927	_errDivZero:
1928		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1929		bic sp, sp, #0x7
1930		adr r0, .L._errDivZero_str0
1931		bl _prints
1932		mov r0, #255
1933		bl _exit
1934	
1935	@ length of .L._errNull_str0
1936		.word 45
1937	.L._errNull_str0:
1938		.asciz "fatal error: null pair dereferenced or freed\n"
1939	.align 4
1940	_errNull:
1941		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1942		bic sp, sp, #0x7
1943		adr r0, .L._errNull_str0
1944		bl _prints
1945		mov r0, #255
1946		bl _exit
1947	
1948	@ length of .L._errOutOfBounds_str0
1949		.word 42
1950	.L._errOutOfBounds_str0:
1951		.asciz "fatal error: array index %d out of bounds\n"
1952	.align 4
1953	_errOutOfBounds:
1954		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1955		bic sp, sp, #0x7
1956		adr r0, .L._errOutOfBounds_str0
1957		bl printf
1958		mov r0, #0
1959		bl fflush
1960		mov r0, #255
1961		bl _exit
1962	
1963	@ length of .L._errOverflow_str0
1964		.word 52
1965	.L._errOverflow_str0:
1966		.asciz "fatal error: integer overflow or underflow occurred\n"
1967	.align 4
1968	_errOverflow:
1969		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1970		bic sp, sp, #0x7
1971		adr r0, .L._errOverflow_str0
1972		bl _prints
1973		mov r0, #255
1974		bl _exit
===========================================================
-- Finished

