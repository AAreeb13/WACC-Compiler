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
1	// length of .L.str0
2		.word 0
3	.L.str0:
4		.asciz ""
5	// length of .L.str1
6		.word 43
7	.L.str1:
8		.asciz "==========================================="
9	// length of .L.str2
10		.word 43
11	.L.str2:
12		.asciz "========== Hash Table Program ============="
13	// length of .L.str3
14		.word 43
15	.L.str3:
16		.asciz "=                                         ="
17	// length of .L.str4
18		.word 43
19	.L.str4:
20		.asciz "= Please choose the following options:    ="
21	// length of .L.str5
22		.word 43
23	.L.str5:
24		.asciz "= a: insert an integer                    ="
25	// length of .L.str6
26		.word 43
27	.L.str6:
28		.asciz "= b: find an integer                      ="
29	// length of .L.str7
30		.word 43
31	.L.str7:
32		.asciz "= c: count the integers                   ="
33	// length of .L.str8
34		.word 43
35	.L.str8:
36		.asciz "= d: print all integers                   ="
37	// length of .L.str9
38		.word 43
39	.L.str9:
40		.asciz "= e: remove an integer                    ="
41	// length of .L.str10
42		.word 43
43	.L.str10:
44		.asciz "= f: remove all integers                  ="
45	// length of .L.str11
46		.word 43
47	.L.str11:
48		.asciz "= g: exit                                 ="
49	// length of .L.str12
50		.word 15
51	.L.str12:
52		.asciz "Your decision: "
53	// length of .L.str13
54		.word 18
55	.L.str13:
56		.asciz "You have entered: "
57	// length of .L.str14
58		.word 36
59	.L.str14:
60		.asciz " which is invalid, please try again."
61	// length of .L.str15
62		.word 35
63	.L.str15:
64		.asciz "Please enter an integer to insert: "
65	// length of .L.str16
66		.word 51
67	.L.str16:
68		.asciz "The integer is already there. No insertion is made."
69	// length of .L.str17
70		.word 43
71	.L.str17:
72		.asciz "Successfully insert it. The integer is new."
73	// length of .L.str18
74		.word 33
75	.L.str18:
76		.asciz "Please enter an integer to find: "
77	// length of .L.str19
78		.word 25
79	.L.str19:
80		.asciz "The integer is not found."
81	// length of .L.str20
82		.word 17
83	.L.str20:
84		.asciz "Find the integer."
85	// length of .L.str21
86		.word 10
87	.L.str21:
88		.asciz "There are "
89	// length of .L.str22
90		.word 10
91	.L.str22:
92		.asciz " integers."
93	// length of .L.str23
94		.word 24
95	.L.str23:
96		.asciz "There is only 1 integer."
97	// length of .L.str24
98		.word 23
99	.L.str24:
100		.asciz "Here are the integers: "
101	// length of .L.str25
102		.word 35
103	.L.str25:
104		.asciz "Please enter an integer to remove: "
105	// length of .L.str26
106		.word 29
107	.L.str26:
108		.asciz "The integer has been removed."
109	// length of .L.str27
110		.word 31
111	.L.str27:
112		.asciz "All integers have been removed."
113	// length of .L.str28
114		.word 23
115	.L.str28:
116		.asciz "Error: unknown choice ("
117	// length of .L.str29
118		.word 1
119	.L.str29:
120		.asciz ")"
121	// length of .L.str30
122		.word 13
123	.L.str30:
124		.asciz "Goodbye Human"
125	.align 4
126	.text
127	.global main
128	main:
129		// push {fp, lr}
130		stp fp, lr, [sp, #-16]!
131		// push {x19, x20, x21, x22, x23}
132		stp x19, x20, [sp, #-48]!
133		stp x21, x22, [sp, #16]
134		stp x23, xzr, [sp, #32]
135		mov fp, sp
136		// Stack pointer unchanged, no stack allocated variables
137		// 13 element array
138		mov w0, #108
139		bl _malloc
140		mov x16, x0
141		// array pointers are shifted forwards by 4 bytes (to account for size)
142		add x16, x16, #4
143		mov x8, #13
144		str w8, [x16, #-4]
145		mov x8, #0
146		str x8, [x16, #0]
147		mov x8, #0
148		str x8, [x16, #8]
149		mov x8, #0
150		str x8, [x16, #16]
151		mov x8, #0
152		str x8, [x16, #24]
153		mov x8, #0
154		str x8, [x16, #32]
155		mov x8, #0
156		str x8, [x16, #40]
157		mov x8, #0
158		str x8, [x16, #48]
159		mov x8, #0
160		str x8, [x16, #56]
161		mov x8, #0
162		str x8, [x16, #64]
163		mov x8, #0
164		str x8, [x16, #72]
165		mov x8, #0
166		str x8, [x16, #80]
167		mov x8, #0
168		str x8, [x16, #88]
169		mov x8, #0
170		str x8, [x16, #96]
171		mov x8, x16
172		mov x19, x8
173		// Stack pointer unchanged, no stack allocated arguments
174		mov x8, x19
175		mov x0, x8
176		bl wacc_init
177		mov x16, x0
178		// Stack pointer unchanged, no stack allocated arguments
179		mov x8, x16
180		mov x20, x8
181		mov x8, #1
182		mov x21, x8
183		b .L39
184	.L40:
185		// Stack pointer unchanged, no stack allocated variables
186		// Stack pointer unchanged, no stack allocated arguments
187		bl wacc_printMenu
188		mov x16, x0
189		// Stack pointer unchanged, no stack allocated arguments
190		mov x8, x16
191		mov x22, x8
192		cmp x22, #97
193		b.eq .L41
194		cmp x22, #98
195		b.eq .L43
196		cmp x22, #99
197		b.eq .L45
198		cmp x22, #100
199		b.eq .L47
200		cmp x22, #101
201		b.eq .L49
202		cmp x22, #102
203		b.eq .L51
204		cmp x22, #103
205		b.eq .L53
206		// Stack pointer unchanged, no stack allocated arguments
207		adrp x8, .L.str28
208		add x8, x8, :lo12:.L.str28
209		// push {x8}
210		stp x8, xzr, [sp, #-16]!
211		// pop {x8}
212		ldp x8, xzr, [sp], #16
213		mov x8, x8
214		mov x0, x8
215		// statement primitives do not return results (but will clobber r0/rax)
216		bl _prints
217		// Stack pointer unchanged, no stack allocated arguments
218		mov x8, x22
219		mov x0, x8
220		// statement primitives do not return results (but will clobber r0/rax)
221		bl _printc
222		// Stack pointer unchanged, no stack allocated arguments
223		adrp x8, .L.str29
224		add x8, x8, :lo12:.L.str29
225		// push {x8}
226		stp x8, xzr, [sp, #-16]!
227		// pop {x8}
228		ldp x8, xzr, [sp], #16
229		mov x8, x8
230		mov x0, x8
231		// statement primitives do not return results (but will clobber r0/rax)
232		bl _prints
233		bl _println
234		// Stack pointer unchanged, no stack allocated arguments
235		mov x8, #-1
236		mov x0, x8
237		// statement primitives do not return results (but will clobber r0/rax)
238		bl exit
239		b .L54
240	.L53:
241		// Stack pointer unchanged, no stack allocated arguments
242		adrp x8, .L.str30
243		add x8, x8, :lo12:.L.str30
244		// push {x8}
245		stp x8, xzr, [sp, #-16]!
246		// pop {x8}
247		ldp x8, xzr, [sp], #16
248		mov x8, x8
249		mov x0, x8
250		// statement primitives do not return results (but will clobber r0/rax)
251		bl _prints
252		bl _println
253		mov x8, #0
254		mov x21, x8
255	.L54:
256		b .L52
257	.L51:
258		// Stack pointer unchanged, no stack allocated variables
259		// Stack pointer unchanged, no stack allocated arguments
260		mov x8, x19
261		mov x0, x8
262		bl wacc_handleMenuRemoveAll
263		mov x16, x0
264		// Stack pointer unchanged, no stack allocated arguments
265		mov x8, x16
266		mov x23, x8
267		// Stack pointer unchanged, no stack allocated variables
268	.L52:
269		b .L50
270	.L49:
271		// Stack pointer unchanged, no stack allocated variables
272		// Stack pointer unchanged, no stack allocated arguments
273		mov x8, x19
274		mov x0, x8
275		bl wacc_handleMenuRemove
276		mov x16, x0
277		// Stack pointer unchanged, no stack allocated arguments
278		mov x8, x16
279		mov x23, x8
280		// Stack pointer unchanged, no stack allocated variables
281	.L50:
282		b .L48
283	.L47:
284		// Stack pointer unchanged, no stack allocated variables
285		// Stack pointer unchanged, no stack allocated arguments
286		mov x8, x19
287		mov x0, x8
288		bl wacc_handleMenuPrint
289		mov x16, x0
290		// Stack pointer unchanged, no stack allocated arguments
291		mov x8, x16
292		mov x23, x8
293		// Stack pointer unchanged, no stack allocated variables
294	.L48:
295		b .L46
296	.L45:
297		// Stack pointer unchanged, no stack allocated variables
298		// Stack pointer unchanged, no stack allocated arguments
299		mov x8, x19
300		mov x0, x8
301		bl wacc_handleMenuCount
302		mov x16, x0
303		// Stack pointer unchanged, no stack allocated arguments
304		mov x8, x16
305		mov x23, x8
306		// Stack pointer unchanged, no stack allocated variables
307	.L46:
308		b .L44
309	.L43:
310		// Stack pointer unchanged, no stack allocated variables
311		// Stack pointer unchanged, no stack allocated arguments
312		mov x8, x19
313		mov x0, x8
314		bl wacc_handleMenuFind
315		mov x16, x0
316		// Stack pointer unchanged, no stack allocated arguments
317		mov x8, x16
318		mov x23, x8
319		// Stack pointer unchanged, no stack allocated variables
320	.L44:
321		b .L42
322	.L41:
323		// Stack pointer unchanged, no stack allocated variables
324		// Stack pointer unchanged, no stack allocated arguments
325		mov x8, x19
326		mov x0, x8
327		bl wacc_handleMenuInsert
328		mov x16, x0
329		// Stack pointer unchanged, no stack allocated arguments
330		mov x8, x16
331		mov x23, x8
332		// Stack pointer unchanged, no stack allocated variables
333	.L42:
334		// Stack pointer unchanged, no stack allocated variables
335	.L39:
336		cmp x21, #1
337		b.eq .L40
338		// Stack pointer unchanged, no stack allocated variables
339		mov x0, #0
340		// pop {x19, x20, x21, x22, x23}
341		ldp x21, x22, [sp, #16]
342		ldp x23, xzr, [sp, #32]
343		ldp x19, x20, [sp], #48
344		// pop {fp, lr}
345		ldp fp, lr, [sp], #16
346		ret
347	
348	wacc_init:
349		// push {fp, lr}
350		stp fp, lr, [sp, #-16]!
351		// push {x19, x20}
352		stp x19, x20, [sp, #-16]!
353		mov fp, sp
354		// Stack pointer unchanged, no stack allocated variables
355		ldrsw x8, [x0, #-4]
356		mov x8, x8
357		mov x19, x8
358		mov x8, #0
359		mov x20, x8
360		b .L0
361	.L1:
362		mov w17, w20
363		mov x8, #0
364		mov x7, x0
365		bl _arrStore8
366		adds w8, w20, #1
367		b.vs _errOverflow
368		sxtw x8, w8
369		// push {x8}
370		stp x8, xzr, [sp, #-16]!
371		// pop {x8}
372		ldp x8, xzr, [sp], #16
373		mov x8, x8
374		mov x20, x8
375	.L0:
376		cmp x20, x19
377		b.lt .L1
378		mov x0, #1
379		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
380		mov sp, fp
381		// pop {x19, x20}
382		ldp x19, x20, [sp], #16
383		// pop {fp, lr}
384		ldp fp, lr, [sp], #16
385		ret
386		// Stack pointer unchanged, no stack allocated variables
387		// 'ere be dragons: this is 100% dead code, functions always end in returns!
388	
389	wacc_contain:
390		// push {fp, lr}
391		stp fp, lr, [sp, #-16]!
392		// push {x19, x20}
393		stp x19, x20, [sp, #-16]!
394		mov fp, sp
395		// Stack pointer unchanged, no stack allocated variables
396		// push {x0, x1}
397		stp x0, x1, [sp, #-16]!
398		// Set up X16 as a temporary second base pointer for the caller saved things
399		mov x16, sp
400		// Stack pointer unchanged, no stack allocated arguments
401		mov x8, x0
402		mov x0, x8
403		mov x8, x1
404		mov x1, x8
405		bl wacc_calculateIndex
406		mov x16, x0
407		// Stack pointer unchanged, no stack allocated arguments
408		// pop {x0, x1}
409		ldp x0, x1, [sp], #16
410		mov x8, x16
411		mov x19, x8
412		// push {x0, x1}
413		stp x0, x1, [sp, #-16]!
414		// Set up X16 as a temporary second base pointer for the caller saved things
415		mov x16, sp
416		// Stack pointer unchanged, no stack allocated arguments
417		mov w17, w19
418		mov x7, x0
419		bl _arrLoad8
420		mov x8, x7
421		mov x8, x8
422		mov x0, x8
423		mov x8, x1
424		mov x1, x8
425		bl wacc_findNode
426		mov x16, x0
427		// Stack pointer unchanged, no stack allocated arguments
428		// pop {x0, x1}
429		ldp x0, x1, [sp], #16
430		mov x8, x16
431		mov x20, x8
432		cmp x20, #0
433		cset x8, ne
434		// push {x8}
435		stp x8, xzr, [sp, #-16]!
436		// pop {x0}
437		ldp x0, xzr, [sp], #16
438		mov x0, x0
439		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
440		mov sp, fp
441		// pop {x19, x20}
442		ldp x19, x20, [sp], #16
443		// pop {fp, lr}
444		ldp fp, lr, [sp], #16
445		ret
446		// Stack pointer unchanged, no stack allocated variables
447		// 'ere be dragons: this is 100% dead code, functions always end in returns!
448	
449	wacc_insertIfNotContain:
450		// push {fp, lr}
451		stp fp, lr, [sp, #-16]!
452		// push {x19, x20, x21}
453		stp x19, x20, [sp, #-32]!
454		stp x21, xzr, [sp, #16]
455		mov fp, sp
456		// Stack pointer unchanged, no stack allocated variables
457		// push {x0, x1}
458		stp x0, x1, [sp, #-16]!
459		// Set up X16 as a temporary second base pointer for the caller saved things
460		mov x16, sp
461		// Stack pointer unchanged, no stack allocated arguments
462		mov x8, x0
463		mov x0, x8
464		mov x8, x1
465		mov x1, x8
466		bl wacc_calculateIndex
467		mov x16, x0
468		// Stack pointer unchanged, no stack allocated arguments
469		// pop {x0, x1}
470		ldp x0, x1, [sp], #16
471		mov x8, x16
472		mov x19, x8
473		// push {x0, x1}
474		stp x0, x1, [sp, #-16]!
475		// Set up X16 as a temporary second base pointer for the caller saved things
476		mov x16, sp
477		// Stack pointer unchanged, no stack allocated arguments
478		mov w17, w19
479		mov x7, x0
480		bl _arrLoad8
481		mov x8, x7
482		mov x8, x8
483		mov x0, x8
484		mov x8, x1
485		mov x1, x8
486		bl wacc_findNode
487		mov x16, x0
488		// Stack pointer unchanged, no stack allocated arguments
489		// pop {x0, x1}
490		ldp x0, x1, [sp], #16
491		mov x8, x16
492		mov x20, x8
493		cmp x20, #0
494		b.ne .L2
495		// Stack pointer unchanged, no stack allocated variables
496		// push {x0, x1}
497		stp x0, x1, [sp, #-16]!
498		mov w0, #16
499		bl _malloc
500		mov x16, x0
501		// pop {x0, x1}
502		ldp x0, x1, [sp], #16
503		mov x8, x1
504		str x8, [x16, #0]
505		mov w17, w19
506		mov x7, x0
507		bl _arrLoad8
508		mov x8, x7
509		mov x8, x8
510		str x8, [x16, #8]
511		mov x8, x16
512		mov x21, x8
513		mov w17, w19
514		mov x8, x21
515		mov x7, x0
516		bl _arrStore8
517		mov x0, #1
518		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
519		mov sp, fp
520		// pop {x19, x20, x21}
521		ldp x21, xzr, [sp, #16]
522		ldp x19, x20, [sp], #32
523		// pop {fp, lr}
524		ldp fp, lr, [sp], #16
525		ret
526		// Stack pointer unchanged, no stack allocated variables
527		b .L3
528	.L2:
529		mov x0, #0
530		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
531		mov sp, fp
532		// pop {x19, x20, x21}
533		ldp x21, xzr, [sp, #16]
534		ldp x19, x20, [sp], #32
535		// pop {fp, lr}
536		ldp fp, lr, [sp], #16
537		ret
538	.L3:
539		// Stack pointer unchanged, no stack allocated variables
540		// 'ere be dragons: this is 100% dead code, functions always end in returns!
541	
542	wacc_remove:
543		// push {fp, lr}
544		stp fp, lr, [sp, #-16]!
545		// push {x19, x20}
546		stp x19, x20, [sp, #-16]!
547		mov fp, sp
548		// Stack pointer unchanged, no stack allocated variables
549		// push {x0, x1}
550		stp x0, x1, [sp, #-16]!
551		// Set up X16 as a temporary second base pointer for the caller saved things
552		mov x16, sp
553		// Stack pointer unchanged, no stack allocated arguments
554		mov x8, x0
555		mov x0, x8
556		mov x8, x1
557		mov x1, x8
558		bl wacc_calculateIndex
559		mov x16, x0
560		// Stack pointer unchanged, no stack allocated arguments
561		// pop {x0, x1}
562		ldp x0, x1, [sp], #16
563		mov x8, x16
564		mov x19, x8
565		// push {x0, x1}
566		stp x0, x1, [sp, #-16]!
567		// Set up X16 as a temporary second base pointer for the caller saved things
568		mov x16, sp
569		// Stack pointer unchanged, no stack allocated arguments
570		mov w17, w19
571		mov x7, x0
572		bl _arrLoad8
573		mov x8, x7
574		mov x8, x8
575		mov x0, x8
576		mov x8, x1
577		mov x1, x8
578		bl wacc_findNode
579		mov x16, x0
580		// Stack pointer unchanged, no stack allocated arguments
581		// pop {x0, x1}
582		ldp x0, x1, [sp], #16
583		mov x8, x16
584		mov x20, x8
585		cmp x20, #0
586		b.eq .L4
587		// push {x0, x1}
588		stp x0, x1, [sp, #-16]!
589		// Set up X16 as a temporary second base pointer for the caller saved things
590		mov x16, sp
591		// Stack pointer unchanged, no stack allocated arguments
592		mov w17, w19
593		mov x7, x0
594		bl _arrLoad8
595		mov x8, x7
596		mov x8, x8
597		mov x0, x8
598		mov x8, x20
599		mov x1, x8
600		bl wacc_removeNode
601		mov x16, x0
602		// Stack pointer unchanged, no stack allocated arguments
603		// pop {x0, x1}
604		ldp x0, x1, [sp], #16
605		mov w17, w19
606		mov x8, x16
607		mov x7, x0
608		bl _arrStore8
609		mov x0, #1
610		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
611		mov sp, fp
612		// pop {x19, x20}
613		ldp x19, x20, [sp], #16
614		// pop {fp, lr}
615		ldp fp, lr, [sp], #16
616		ret
617		b .L5
618	.L4:
619		mov x0, #0
620		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
621		mov sp, fp
622		// pop {x19, x20}
623		ldp x19, x20, [sp], #16
624		// pop {fp, lr}
625		ldp fp, lr, [sp], #16
626		ret
627	.L5:
628		// Stack pointer unchanged, no stack allocated variables
629		// 'ere be dragons: this is 100% dead code, functions always end in returns!
630	
631	wacc_removeAll:
632		// push {fp, lr}
633		stp fp, lr, [sp, #-16]!
634		// push {x19, x20, x21, x22}
635		stp x19, x20, [sp, #-32]!
636		stp x21, x22, [sp, #16]
637		mov fp, sp
638		// Stack pointer unchanged, no stack allocated variables
639		ldrsw x8, [x0, #-4]
640		mov x8, x8
641		mov x19, x8
642		mov x8, #0
643		mov x20, x8
644		b .L6
645	.L7:
646		// Stack pointer unchanged, no stack allocated variables
647		mov w17, w20
648		mov x7, x0
649		bl _arrLoad8
650		mov x8, x7
651		mov x8, x8
652		mov x21, x8
653		b .L8
654	.L9:
655		// Stack pointer unchanged, no stack allocated variables
656		cbz x21, _errNull
657		ldr x8, [x21, #8]
658		mov x8, x8
659		mov x22, x8
660		// push {x0}
661		stp x0, xzr, [sp, #-16]!
662		// Set up X16 as a temporary second base pointer for the caller saved things
663		mov x16, sp
664		// Stack pointer unchanged, no stack allocated arguments
665		mov x8, x21
666		mov x0, x8
667		// statement primitives do not return results (but will clobber r0/rax)
668		bl _freepair
669		// pop {x0}
670		ldp x0, xzr, [sp], #16
671		mov x8, x22
672		mov x21, x8
673		// Stack pointer unchanged, no stack allocated variables
674	.L8:
675		cmp x21, #0
676		b.ne .L9
677		mov w17, w20
678		mov x8, #0
679		mov x7, x0
680		bl _arrStore8
681		adds w8, w20, #1
682		b.vs _errOverflow
683		sxtw x8, w8
684		// push {x8}
685		stp x8, xzr, [sp, #-16]!
686		// pop {x8}
687		ldp x8, xzr, [sp], #16
688		mov x8, x8
689		mov x20, x8
690		// Stack pointer unchanged, no stack allocated variables
691	.L6:
692		cmp x20, x19
693		b.lt .L7
694		mov x0, #1
695		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
696		mov sp, fp
697		// pop {x19, x20, x21, x22}
698		ldp x21, x22, [sp, #16]
699		ldp x19, x20, [sp], #32
700		// pop {fp, lr}
701		ldp fp, lr, [sp], #16
702		ret
703		// Stack pointer unchanged, no stack allocated variables
704		// 'ere be dragons: this is 100% dead code, functions always end in returns!
705	
706	wacc_count:
707		// push {fp, lr}
708		stp fp, lr, [sp, #-16]!
709		// push {x19, x20, x21, x22}
710		stp x19, x20, [sp, #-32]!
711		stp x21, x22, [sp, #16]
712		mov fp, sp
713		// Stack pointer unchanged, no stack allocated variables
714		ldrsw x8, [x0, #-4]
715		mov x8, x8
716		mov x19, x8
717		mov x8, #0
718		mov x20, x8
719		mov x8, #0
720		mov x21, x8
721		b .L10
722	.L11:
723		// Stack pointer unchanged, no stack allocated variables
724		// push {x0}
725		stp x0, xzr, [sp, #-16]!
726		// Set up X16 as a temporary second base pointer for the caller saved things
727		mov x16, sp
728		// Stack pointer unchanged, no stack allocated arguments
729		mov w17, w21
730		mov x7, x0
731		bl _arrLoad8
732		mov x8, x7
733		mov x8, x8
734		mov x0, x8
735		bl wacc_countNodes
736		mov x16, x0
737		// Stack pointer unchanged, no stack allocated arguments
738		// pop {x0}
739		ldp x0, xzr, [sp], #16
740		mov x8, x16
741		mov x22, x8
742		adds w8, w20, w22
743		b.vs _errOverflow
744		sxtw x8, w8
745		// push {x8}
746		stp x8, xzr, [sp, #-16]!
747		// pop {x8}
748		ldp x8, xzr, [sp], #16
749		mov x8, x8
750		mov x20, x8
751		adds w8, w21, #1
752		b.vs _errOverflow
753		sxtw x8, w8
754		// push {x8}
755		stp x8, xzr, [sp, #-16]!
756		// pop {x8}
757		ldp x8, xzr, [sp], #16
758		mov x8, x8
759		mov x21, x8
760		// Stack pointer unchanged, no stack allocated variables
761	.L10:
762		cmp x21, x19
763		b.lt .L11
764		mov x0, x20
765		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
766		mov sp, fp
767		// pop {x19, x20, x21, x22}
768		ldp x21, x22, [sp, #16]
769		ldp x19, x20, [sp], #32
770		// pop {fp, lr}
771		ldp fp, lr, [sp], #16
772		ret
773		// Stack pointer unchanged, no stack allocated variables
774		// 'ere be dragons: this is 100% dead code, functions always end in returns!
775	
776	wacc_printAll:
777		// push {fp, lr}
778		stp fp, lr, [sp, #-16]!
779		// push {x19, x20, x21}
780		stp x19, x20, [sp, #-32]!
781		stp x21, xzr, [sp, #16]
782		mov fp, sp
783		// Stack pointer unchanged, no stack allocated variables
784		ldrsw x8, [x0, #-4]
785		mov x8, x8
786		mov x19, x8
787		mov x8, #0
788		mov x20, x8
789		b .L12
790	.L13:
791		// Stack pointer unchanged, no stack allocated variables
792		// push {x0}
793		stp x0, xzr, [sp, #-16]!
794		// Set up X16 as a temporary second base pointer for the caller saved things
795		mov x16, sp
796		// Stack pointer unchanged, no stack allocated arguments
797		mov w17, w20
798		mov x7, x0
799		bl _arrLoad8
800		mov x8, x7
801		mov x8, x8
802		mov x0, x8
803		bl wacc_printAllNodes
804		mov x16, x0
805		// Stack pointer unchanged, no stack allocated arguments
806		// pop {x0}
807		ldp x0, xzr, [sp], #16
808		mov x8, x16
809		mov x21, x8
810		adds w8, w20, #1
811		b.vs _errOverflow
812		sxtw x8, w8
813		// push {x8}
814		stp x8, xzr, [sp, #-16]!
815		// pop {x8}
816		ldp x8, xzr, [sp], #16
817		mov x8, x8
818		mov x20, x8
819		// Stack pointer unchanged, no stack allocated variables
820	.L12:
821		cmp x20, x19
822		b.lt .L13
823		// push {x0}
824		stp x0, xzr, [sp, #-16]!
825		// Set up X16 as a temporary second base pointer for the caller saved things
826		mov x16, sp
827		// Stack pointer unchanged, no stack allocated arguments
828		adrp x8, .L.str0
829		add x8, x8, :lo12:.L.str0
830		// push {x8}
831		stp x8, xzr, [sp, #-16]!
832		// pop {x8}
833		ldp x8, xzr, [sp], #16
834		mov x8, x8
835		mov x0, x8
836		// statement primitives do not return results (but will clobber r0/rax)
837		bl _prints
838		bl _println
839		// pop {x0}
840		ldp x0, xzr, [sp], #16
841		mov x0, #1
842		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
843		mov sp, fp
844		// pop {x19, x20, x21}
845		ldp x21, xzr, [sp, #16]
846		ldp x19, x20, [sp], #32
847		// pop {fp, lr}
848		ldp fp, lr, [sp], #16
849		ret
850		// Stack pointer unchanged, no stack allocated variables
851		// 'ere be dragons: this is 100% dead code, functions always end in returns!
852	
853	wacc_calculateIndex:
854		// push {fp, lr}
855		stp fp, lr, [sp, #-16]!
856		// push {x19}
857		stp x19, xzr, [sp, #-16]!
858		mov fp, sp
859		// Stack pointer unchanged, no stack allocated variables
860		ldrsw x8, [x0, #-4]
861		mov x8, x8
862		mov x19, x8
863		cbz x19, _errDivZero
864		sdiv w17, w1, w19
865		msub w8, w17, w19, w1
866		sxtw x8, w8
867		// push {x8}
868		stp x8, xzr, [sp, #-16]!
869		// pop {x0}
870		ldp x0, xzr, [sp], #16
871		mov x0, x0
872		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
873		mov sp, fp
874		// pop {x19}
875		ldp x19, xzr, [sp], #16
876		// pop {fp, lr}
877		ldp fp, lr, [sp], #16
878		ret
879		// Stack pointer unchanged, no stack allocated variables
880		// 'ere be dragons: this is 100% dead code, functions always end in returns!
881	
882	wacc_findNode:
883		// push {fp, lr}
884		stp fp, lr, [sp, #-16]!
885		// push {x19}
886		stp x19, xzr, [sp, #-16]!
887		mov fp, sp
888		b .L14
889	.L15:
890		// Stack pointer unchanged, no stack allocated variables
891		cbz x0, _errNull
892		ldr x8, [x0, #0]
893		mov x8, x8
894		mov x19, x8
895		cmp x19, x1
896		b.eq .L16
897		cbz x0, _errNull
898		ldr x8, [x0, #8]
899		mov x8, x8
900		// push {x8}
901		stp x8, xzr, [sp, #-16]!
902		// pop {x8}
903		ldp x8, xzr, [sp], #16
904		mov x8, x8
905		mov x0, x8
906		b .L17
907	.L16:
908		mov x0, x0
909		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
910		mov sp, fp
911		// pop {x19}
912		ldp x19, xzr, [sp], #16
913		// pop {fp, lr}
914		ldp fp, lr, [sp], #16
915		ret
916	.L17:
917		// Stack pointer unchanged, no stack allocated variables
918	.L14:
919		cmp x0, #0
920		b.ne .L15
921		mov x0, #0
922		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
923		mov sp, fp
924		// pop {x19}
925		ldp x19, xzr, [sp], #16
926		// pop {fp, lr}
927		ldp fp, lr, [sp], #16
928		ret
929		// 'ere be dragons: this is 100% dead code, functions always end in returns!
930	
931	wacc_removeNode:
932		// push {fp, lr}
933		stp fp, lr, [sp, #-16]!
934		// push {x19}
935		stp x19, xzr, [sp, #-16]!
936		mov fp, sp
937		cmp x0, #0
938		b.eq .L18
939		cmp x0, x1
940		b.eq .L20
941		// Stack pointer unchanged, no stack allocated variables
942		cbz x0, _errNull
943		ldr x8, [x0, #8]
944		mov x8, x8
945		mov x19, x8
946		// push {x0, x1}
947		stp x0, x1, [sp, #-16]!
948		// Set up X16 as a temporary second base pointer for the caller saved things
949		mov x16, sp
950		// Stack pointer unchanged, no stack allocated arguments
951		mov x8, x19
952		mov x0, x8
953		mov x8, x1
954		mov x1, x8
955		bl wacc_removeNode
956		mov x16, x0
957		// Stack pointer unchanged, no stack allocated arguments
958		// pop {x0, x1}
959		ldp x0, x1, [sp], #16
960		cbz x0, _errNull
961		mov x8, x16
962		str x8, [x0, #8]
963		mov x0, x0
964		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
965		mov sp, fp
966		// pop {x19}
967		ldp x19, xzr, [sp], #16
968		// pop {fp, lr}
969		ldp fp, lr, [sp], #16
970		ret
971		// Stack pointer unchanged, no stack allocated variables
972		b .L21
973	.L20:
974		cbz x0, _errNull
975		ldr x8, [x0, #8]
976		mov x8, x8
977		// push {x8}
978		stp x8, xzr, [sp, #-16]!
979		// pop {x8}
980		ldp x8, xzr, [sp], #16
981		mov x8, x8
982		mov x0, x8
983		// push {x0, x1}
984		stp x0, x1, [sp, #-16]!
985		// Set up X16 as a temporary second base pointer for the caller saved things
986		mov x16, sp
987		// Stack pointer unchanged, no stack allocated arguments
988		mov x8, x1
989		mov x0, x8
990		// statement primitives do not return results (but will clobber r0/rax)
991		bl _freepair
992		// pop {x0, x1}
993		ldp x0, x1, [sp], #16
994		mov x0, x0
995		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
996		mov sp, fp
997		// pop {x19}
998		ldp x19, xzr, [sp], #16
999		// pop {fp, lr}
1000		ldp fp, lr, [sp], #16
1001		ret
1002	.L21:
1003		b .L19
1004	.L18:
1005		mov x0, #0
1006		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1007		mov sp, fp
1008		// pop {x19}
1009		ldp x19, xzr, [sp], #16
1010		// pop {fp, lr}
1011		ldp fp, lr, [sp], #16
1012		ret
1013	.L19:
1014		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1015	
1016	wacc_countNodes:
1017		// push {fp, lr}
1018		stp fp, lr, [sp, #-16]!
1019		// push {x19}
1020		stp x19, xzr, [sp, #-16]!
1021		mov fp, sp
1022		// Stack pointer unchanged, no stack allocated variables
1023		mov x8, #0
1024		mov x19, x8
1025		b .L22
1026	.L23:
1027		adds w8, w19, #1
1028		b.vs _errOverflow
1029		sxtw x8, w8
1030		// push {x8}
1031		stp x8, xzr, [sp, #-16]!
1032		// pop {x8}
1033		ldp x8, xzr, [sp], #16
1034		mov x8, x8
1035		mov x19, x8
1036		cbz x0, _errNull
1037		ldr x8, [x0, #8]
1038		mov x8, x8
1039		// push {x8}
1040		stp x8, xzr, [sp, #-16]!
1041		// pop {x8}
1042		ldp x8, xzr, [sp], #16
1043		mov x8, x8
1044		mov x0, x8
1045	.L22:
1046		cmp x0, #0
1047		b.ne .L23
1048		mov x0, x19
1049		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1050		mov sp, fp
1051		// pop {x19}
1052		ldp x19, xzr, [sp], #16
1053		// pop {fp, lr}
1054		ldp fp, lr, [sp], #16
1055		ret
1056		// Stack pointer unchanged, no stack allocated variables
1057		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1058	
1059	wacc_printAllNodes:
1060		// push {fp, lr}
1061		stp fp, lr, [sp, #-16]!
1062		// push {x19}
1063		stp x19, xzr, [sp, #-16]!
1064		mov fp, sp
1065		b .L24
1066	.L25:
1067		// Stack pointer unchanged, no stack allocated variables
1068		cbz x0, _errNull
1069		ldr x8, [x0, #0]
1070		mov x8, x8
1071		mov x19, x8
1072		// push {x0}
1073		stp x0, xzr, [sp, #-16]!
1074		// Set up X16 as a temporary second base pointer for the caller saved things
1075		mov x16, sp
1076		// Stack pointer unchanged, no stack allocated arguments
1077		mov x8, x19
1078		mov x0, x8
1079		// statement primitives do not return results (but will clobber r0/rax)
1080		bl _printi
1081		// pop {x0}
1082		ldp x0, xzr, [sp], #16
1083		// push {x0}
1084		stp x0, xzr, [sp, #-16]!
1085		// Set up X16 as a temporary second base pointer for the caller saved things
1086		mov x16, sp
1087		// Stack pointer unchanged, no stack allocated arguments
1088		mov x8, #32
1089		mov x0, x8
1090		// statement primitives do not return results (but will clobber r0/rax)
1091		bl _printc
1092		// pop {x0}
1093		ldp x0, xzr, [sp], #16
1094		cbz x0, _errNull
1095		ldr x8, [x0, #8]
1096		mov x8, x8
1097		// push {x8}
1098		stp x8, xzr, [sp, #-16]!
1099		// pop {x8}
1100		ldp x8, xzr, [sp], #16
1101		mov x8, x8
1102		mov x0, x8
1103		// Stack pointer unchanged, no stack allocated variables
1104	.L24:
1105		cmp x0, #0
1106		b.ne .L25
1107		mov x0, #1
1108		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1109		mov sp, fp
1110		// pop {x19}
1111		ldp x19, xzr, [sp], #16
1112		// pop {fp, lr}
1113		ldp fp, lr, [sp], #16
1114		ret
1115		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1116	
1117	wacc_printMenu:
1118		// push {fp, lr}
1119		stp fp, lr, [sp, #-16]!
1120		// push {x19, x20, x21, x22}
1121		stp x19, x20, [sp, #-32]!
1122		stp x21, x22, [sp, #16]
1123		mov fp, sp
1124		// Stack pointer unchanged, no stack allocated variables
1125		// Stack pointer unchanged, no stack allocated arguments
1126		adrp x8, .L.str1
1127		add x8, x8, :lo12:.L.str1
1128		// push {x8}
1129		stp x8, xzr, [sp, #-16]!
1130		// pop {x8}
1131		ldp x8, xzr, [sp], #16
1132		mov x8, x8
1133		mov x0, x8
1134		// statement primitives do not return results (but will clobber r0/rax)
1135		bl _prints
1136		bl _println
1137		// Stack pointer unchanged, no stack allocated arguments
1138		adrp x8, .L.str2
1139		add x8, x8, :lo12:.L.str2
1140		// push {x8}
1141		stp x8, xzr, [sp, #-16]!
1142		// pop {x8}
1143		ldp x8, xzr, [sp], #16
1144		mov x8, x8
1145		mov x0, x8
1146		// statement primitives do not return results (but will clobber r0/rax)
1147		bl _prints
1148		bl _println
1149		// Stack pointer unchanged, no stack allocated arguments
1150		adrp x8, .L.str1
1151		add x8, x8, :lo12:.L.str1
1152		// push {x8}
1153		stp x8, xzr, [sp, #-16]!
1154		// pop {x8}
1155		ldp x8, xzr, [sp], #16
1156		mov x8, x8
1157		mov x0, x8
1158		// statement primitives do not return results (but will clobber r0/rax)
1159		bl _prints
1160		bl _println
1161		// Stack pointer unchanged, no stack allocated arguments
1162		adrp x8, .L.str3
1163		add x8, x8, :lo12:.L.str3
1164		// push {x8}
1165		stp x8, xzr, [sp, #-16]!
1166		// pop {x8}
1167		ldp x8, xzr, [sp], #16
1168		mov x8, x8
1169		mov x0, x8
1170		// statement primitives do not return results (but will clobber r0/rax)
1171		bl _prints
1172		bl _println
1173		// Stack pointer unchanged, no stack allocated arguments
1174		adrp x8, .L.str4
1175		add x8, x8, :lo12:.L.str4
1176		// push {x8}
1177		stp x8, xzr, [sp, #-16]!
1178		// pop {x8}
1179		ldp x8, xzr, [sp], #16
1180		mov x8, x8
1181		mov x0, x8
1182		// statement primitives do not return results (but will clobber r0/rax)
1183		bl _prints
1184		bl _println
1185		// Stack pointer unchanged, no stack allocated arguments
1186		adrp x8, .L.str3
1187		add x8, x8, :lo12:.L.str3
1188		// push {x8}
1189		stp x8, xzr, [sp, #-16]!
1190		// pop {x8}
1191		ldp x8, xzr, [sp], #16
1192		mov x8, x8
1193		mov x0, x8
1194		// statement primitives do not return results (but will clobber r0/rax)
1195		bl _prints
1196		bl _println
1197		// Stack pointer unchanged, no stack allocated arguments
1198		adrp x8, .L.str5
1199		add x8, x8, :lo12:.L.str5
1200		// push {x8}
1201		stp x8, xzr, [sp, #-16]!
1202		// pop {x8}
1203		ldp x8, xzr, [sp], #16
1204		mov x8, x8
1205		mov x0, x8
1206		// statement primitives do not return results (but will clobber r0/rax)
1207		bl _prints
1208		bl _println
1209		// Stack pointer unchanged, no stack allocated arguments
1210		adrp x8, .L.str6
1211		add x8, x8, :lo12:.L.str6
1212		// push {x8}
1213		stp x8, xzr, [sp, #-16]!
1214		// pop {x8}
1215		ldp x8, xzr, [sp], #16
1216		mov x8, x8
1217		mov x0, x8
1218		// statement primitives do not return results (but will clobber r0/rax)
1219		bl _prints
1220		bl _println
1221		// Stack pointer unchanged, no stack allocated arguments
1222		adrp x8, .L.str7
1223		add x8, x8, :lo12:.L.str7
1224		// push {x8}
1225		stp x8, xzr, [sp, #-16]!
1226		// pop {x8}
1227		ldp x8, xzr, [sp], #16
1228		mov x8, x8
1229		mov x0, x8
1230		// statement primitives do not return results (but will clobber r0/rax)
1231		bl _prints
1232		bl _println
1233		// Stack pointer unchanged, no stack allocated arguments
1234		adrp x8, .L.str8
1235		add x8, x8, :lo12:.L.str8
1236		// push {x8}
1237		stp x8, xzr, [sp, #-16]!
1238		// pop {x8}
1239		ldp x8, xzr, [sp], #16
1240		mov x8, x8
1241		mov x0, x8
1242		// statement primitives do not return results (but will clobber r0/rax)
1243		bl _prints
1244		bl _println
1245		// Stack pointer unchanged, no stack allocated arguments
1246		adrp x8, .L.str9
1247		add x8, x8, :lo12:.L.str9
1248		// push {x8}
1249		stp x8, xzr, [sp, #-16]!
1250		// pop {x8}
1251		ldp x8, xzr, [sp], #16
1252		mov x8, x8
1253		mov x0, x8
1254		// statement primitives do not return results (but will clobber r0/rax)
1255		bl _prints
1256		bl _println
1257		// Stack pointer unchanged, no stack allocated arguments
1258		adrp x8, .L.str10
1259		add x8, x8, :lo12:.L.str10
1260		// push {x8}
1261		stp x8, xzr, [sp, #-16]!
1262		// pop {x8}
1263		ldp x8, xzr, [sp], #16
1264		mov x8, x8
1265		mov x0, x8
1266		// statement primitives do not return results (but will clobber r0/rax)
1267		bl _prints
1268		bl _println
1269		// Stack pointer unchanged, no stack allocated arguments
1270		adrp x8, .L.str11
1271		add x8, x8, :lo12:.L.str11
1272		// push {x8}
1273		stp x8, xzr, [sp, #-16]!
1274		// pop {x8}
1275		ldp x8, xzr, [sp], #16
1276		mov x8, x8
1277		mov x0, x8
1278		// statement primitives do not return results (but will clobber r0/rax)
1279		bl _prints
1280		bl _println
1281		// Stack pointer unchanged, no stack allocated arguments
1282		adrp x8, .L.str3
1283		add x8, x8, :lo12:.L.str3
1284		// push {x8}
1285		stp x8, xzr, [sp, #-16]!
1286		// pop {x8}
1287		ldp x8, xzr, [sp], #16
1288		mov x8, x8
1289		mov x0, x8
1290		// statement primitives do not return results (but will clobber r0/rax)
1291		bl _prints
1292		bl _println
1293		// Stack pointer unchanged, no stack allocated arguments
1294		adrp x8, .L.str1
1295		add x8, x8, :lo12:.L.str1
1296		// push {x8}
1297		stp x8, xzr, [sp, #-16]!
1298		// pop {x8}
1299		ldp x8, xzr, [sp], #16
1300		mov x8, x8
1301		mov x0, x8
1302		// statement primitives do not return results (but will clobber r0/rax)
1303		bl _prints
1304		bl _println
1305		mov x8, #97
1306		mov x19, x8
1307		mov x8, #103
1308		mov x20, x8
1309		b .L26
1310	.L27:
1311		// Stack pointer unchanged, no stack allocated variables
1312		// Stack pointer unchanged, no stack allocated arguments
1313		adrp x8, .L.str12
1314		add x8, x8, :lo12:.L.str12
1315		// push {x8}
1316		stp x8, xzr, [sp, #-16]!
1317		// pop {x8}
1318		ldp x8, xzr, [sp], #16
1319		mov x8, x8
1320		mov x0, x8
1321		// statement primitives do not return results (but will clobber r0/rax)
1322		bl _prints
1323		mov x8, #0
1324		mov x21, x8
1325		// Stack pointer unchanged, no stack allocated arguments
1326		// load the current value in the destination of the read so it supports defaults
1327		mov x8, x21
1328		mov x0, x8
1329		bl _readc
1330		mov x16, x0
1331		mov x8, x16
1332		mov x21, x8
1333		mov x8, x21
1334		mov x22, x8
1335		cmp x19, x22
1336		cset x8, le
1337		// push {x8}
1338		stp x8, xzr, [sp, #-16]!
1339		// pop {x8}
1340		ldp x8, xzr, [sp], #16
1341		cmp x8, #1
1342		b.ne .L30
1343		cmp x22, x20
1344		cset x8, le
1345		// push {x8}
1346		stp x8, xzr, [sp, #-16]!
1347		// pop {x8}
1348		ldp x8, xzr, [sp], #16
1349		cmp x8, #1
1350	.L30:
1351		b.eq .L28
1352		// Stack pointer unchanged, no stack allocated arguments
1353		adrp x8, .L.str13
1354		add x8, x8, :lo12:.L.str13
1355		// push {x8}
1356		stp x8, xzr, [sp, #-16]!
1357		// pop {x8}
1358		ldp x8, xzr, [sp], #16
1359		mov x8, x8
1360		mov x0, x8
1361		// statement primitives do not return results (but will clobber r0/rax)
1362		bl _prints
1363		// Stack pointer unchanged, no stack allocated arguments
1364		mov x8, x21
1365		mov x0, x8
1366		// statement primitives do not return results (but will clobber r0/rax)
1367		bl _printc
1368		// Stack pointer unchanged, no stack allocated arguments
1369		adrp x8, .L.str14
1370		add x8, x8, :lo12:.L.str14
1371		// push {x8}
1372		stp x8, xzr, [sp, #-16]!
1373		// pop {x8}
1374		ldp x8, xzr, [sp], #16
1375		mov x8, x8
1376		mov x0, x8
1377		// statement primitives do not return results (but will clobber r0/rax)
1378		bl _prints
1379		bl _println
1380		b .L29
1381	.L28:
1382		mov x0, x21
1383		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1384		mov sp, fp
1385		// pop {x19, x20, x21, x22}
1386		ldp x21, x22, [sp, #16]
1387		ldp x19, x20, [sp], #32
1388		// pop {fp, lr}
1389		ldp fp, lr, [sp], #16
1390		ret
1391	.L29:
1392		// Stack pointer unchanged, no stack allocated variables
1393	.L26:
1394		mov x8, #1
1395		cmp x8, #1
1396		b.eq .L27
1397		mov x0, #0
1398		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1399		mov sp, fp
1400		// pop {x19, x20, x21, x22}
1401		ldp x21, x22, [sp, #16]
1402		ldp x19, x20, [sp], #32
1403		// pop {fp, lr}
1404		ldp fp, lr, [sp], #16
1405		ret
1406		// Stack pointer unchanged, no stack allocated variables
1407		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1408	
1409	wacc_askForInt:
1410		// push {fp, lr}
1411		stp fp, lr, [sp, #-16]!
1412		// push {x19}
1413		stp x19, xzr, [sp, #-16]!
1414		mov fp, sp
1415		// Stack pointer unchanged, no stack allocated variables
1416		// push {x0}
1417		stp x0, xzr, [sp, #-16]!
1418		// Set up X16 as a temporary second base pointer for the caller saved things
1419		mov x16, sp
1420		// Stack pointer unchanged, no stack allocated arguments
1421		mov x8, x0
1422		mov x0, x8
1423		// statement primitives do not return results (but will clobber r0/rax)
1424		bl _prints
1425		// pop {x0}
1426		ldp x0, xzr, [sp], #16
1427		mov x8, #0
1428		mov x19, x8
1429		// push {x0}
1430		stp x0, xzr, [sp, #-16]!
1431		// Set up X16 as a temporary second base pointer for the caller saved things
1432		mov x16, sp
1433		// Stack pointer unchanged, no stack allocated arguments
1434		// load the current value in the destination of the read so it supports defaults
1435		mov x8, x19
1436		mov x0, x8
1437		bl _readi
1438		mov x16, x0
1439		// pop {x0}
1440		ldp x0, xzr, [sp], #16
1441		mov x8, x16
1442		mov x19, x8
1443		// push {x0}
1444		stp x0, xzr, [sp, #-16]!
1445		// Set up X16 as a temporary second base pointer for the caller saved things
1446		mov x16, sp
1447		// Stack pointer unchanged, no stack allocated arguments
1448		adrp x8, .L.str13
1449		add x8, x8, :lo12:.L.str13
1450		// push {x8}
1451		stp x8, xzr, [sp, #-16]!
1452		// pop {x8}
1453		ldp x8, xzr, [sp], #16
1454		mov x8, x8
1455		mov x0, x8
1456		// statement primitives do not return results (but will clobber r0/rax)
1457		bl _prints
1458		// pop {x0}
1459		ldp x0, xzr, [sp], #16
1460		// push {x0}
1461		stp x0, xzr, [sp, #-16]!
1462		// Set up X16 as a temporary second base pointer for the caller saved things
1463		mov x16, sp
1464		// Stack pointer unchanged, no stack allocated arguments
1465		mov x8, x19
1466		mov x0, x8
1467		// statement primitives do not return results (but will clobber r0/rax)
1468		bl _printi
1469		bl _println
1470		// pop {x0}
1471		ldp x0, xzr, [sp], #16
1472		mov x0, x19
1473		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1474		mov sp, fp
1475		// pop {x19}
1476		ldp x19, xzr, [sp], #16
1477		// pop {fp, lr}
1478		ldp fp, lr, [sp], #16
1479		ret
1480		// Stack pointer unchanged, no stack allocated variables
1481		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1482	
1483	wacc_handleMenuInsert:
1484		// push {fp, lr}
1485		stp fp, lr, [sp, #-16]!
1486		// push {x19, x20}
1487		stp x19, x20, [sp, #-16]!
1488		mov fp, sp
1489		// Stack pointer unchanged, no stack allocated variables
1490		// push {x0}
1491		stp x0, xzr, [sp, #-16]!
1492		// Set up X16 as a temporary second base pointer for the caller saved things
1493		mov x16, sp
1494		// Stack pointer unchanged, no stack allocated arguments
1495		adrp x8, .L.str15
1496		add x8, x8, :lo12:.L.str15
1497		// push {x8}
1498		stp x8, xzr, [sp, #-16]!
1499		// pop {x8}
1500		ldp x8, xzr, [sp], #16
1501		mov x8, x8
1502		mov x0, x8
1503		bl wacc_askForInt
1504		mov x16, x0
1505		// Stack pointer unchanged, no stack allocated arguments
1506		// pop {x0}
1507		ldp x0, xzr, [sp], #16
1508		mov x8, x16
1509		mov x19, x8
1510		// push {x0}
1511		stp x0, xzr, [sp, #-16]!
1512		// Set up X16 as a temporary second base pointer for the caller saved things
1513		mov x16, sp
1514		// Stack pointer unchanged, no stack allocated arguments
1515		mov x8, x0
1516		mov x0, x8
1517		mov x8, x19
1518		mov x1, x8
1519		bl wacc_insertIfNotContain
1520		mov x16, x0
1521		// Stack pointer unchanged, no stack allocated arguments
1522		// pop {x0}
1523		ldp x0, xzr, [sp], #16
1524		mov x8, x16
1525		mov x20, x8
1526		cmp x20, #1
1527		b.eq .L31
1528		// push {x0}
1529		stp x0, xzr, [sp, #-16]!
1530		// Set up X16 as a temporary second base pointer for the caller saved things
1531		mov x16, sp
1532		// Stack pointer unchanged, no stack allocated arguments
1533		adrp x8, .L.str16
1534		add x8, x8, :lo12:.L.str16
1535		// push {x8}
1536		stp x8, xzr, [sp, #-16]!
1537		// pop {x8}
1538		ldp x8, xzr, [sp], #16
1539		mov x8, x8
1540		mov x0, x8
1541		// statement primitives do not return results (but will clobber r0/rax)
1542		bl _prints
1543		bl _println
1544		// pop {x0}
1545		ldp x0, xzr, [sp], #16
1546		b .L32
1547	.L31:
1548		// push {x0}
1549		stp x0, xzr, [sp, #-16]!
1550		// Set up X16 as a temporary second base pointer for the caller saved things
1551		mov x16, sp
1552		// Stack pointer unchanged, no stack allocated arguments
1553		adrp x8, .L.str17
1554		add x8, x8, :lo12:.L.str17
1555		// push {x8}
1556		stp x8, xzr, [sp, #-16]!
1557		// pop {x8}
1558		ldp x8, xzr, [sp], #16
1559		mov x8, x8
1560		mov x0, x8
1561		// statement primitives do not return results (but will clobber r0/rax)
1562		bl _prints
1563		bl _println
1564		// pop {x0}
1565		ldp x0, xzr, [sp], #16
1566	.L32:
1567		mov x0, #1
1568		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1569		mov sp, fp
1570		// pop {x19, x20}
1571		ldp x19, x20, [sp], #16
1572		// pop {fp, lr}
1573		ldp fp, lr, [sp], #16
1574		ret
1575		// Stack pointer unchanged, no stack allocated variables
1576		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1577	
1578	wacc_handleMenuFind:
1579		// push {fp, lr}
1580		stp fp, lr, [sp, #-16]!
1581		// push {x19, x20}
1582		stp x19, x20, [sp, #-16]!
1583		mov fp, sp
1584		// Stack pointer unchanged, no stack allocated variables
1585		// push {x0}
1586		stp x0, xzr, [sp, #-16]!
1587		// Set up X16 as a temporary second base pointer for the caller saved things
1588		mov x16, sp
1589		// Stack pointer unchanged, no stack allocated arguments
1590		adrp x8, .L.str18
1591		add x8, x8, :lo12:.L.str18
1592		// push {x8}
1593		stp x8, xzr, [sp, #-16]!
1594		// pop {x8}
1595		ldp x8, xzr, [sp], #16
1596		mov x8, x8
1597		mov x0, x8
1598		bl wacc_askForInt
1599		mov x16, x0
1600		// Stack pointer unchanged, no stack allocated arguments
1601		// pop {x0}
1602		ldp x0, xzr, [sp], #16
1603		mov x8, x16
1604		mov x19, x8
1605		// push {x0}
1606		stp x0, xzr, [sp, #-16]!
1607		// Set up X16 as a temporary second base pointer for the caller saved things
1608		mov x16, sp
1609		// Stack pointer unchanged, no stack allocated arguments
1610		mov x8, x0
1611		mov x0, x8
1612		mov x8, x19
1613		mov x1, x8
1614		bl wacc_contain
1615		mov x16, x0
1616		// Stack pointer unchanged, no stack allocated arguments
1617		// pop {x0}
1618		ldp x0, xzr, [sp], #16
1619		mov x8, x16
1620		mov x20, x8
1621		cmp x20, #1
1622		b.eq .L33
1623		// push {x0}
1624		stp x0, xzr, [sp, #-16]!
1625		// Set up X16 as a temporary second base pointer for the caller saved things
1626		mov x16, sp
1627		// Stack pointer unchanged, no stack allocated arguments
1628		adrp x8, .L.str19
1629		add x8, x8, :lo12:.L.str19
1630		// push {x8}
1631		stp x8, xzr, [sp, #-16]!
1632		// pop {x8}
1633		ldp x8, xzr, [sp], #16
1634		mov x8, x8
1635		mov x0, x8
1636		// statement primitives do not return results (but will clobber r0/rax)
1637		bl _prints
1638		bl _println
1639		// pop {x0}
1640		ldp x0, xzr, [sp], #16
1641		b .L34
1642	.L33:
1643		// push {x0}
1644		stp x0, xzr, [sp, #-16]!
1645		// Set up X16 as a temporary second base pointer for the caller saved things
1646		mov x16, sp
1647		// Stack pointer unchanged, no stack allocated arguments
1648		adrp x8, .L.str20
1649		add x8, x8, :lo12:.L.str20
1650		// push {x8}
1651		stp x8, xzr, [sp, #-16]!
1652		// pop {x8}
1653		ldp x8, xzr, [sp], #16
1654		mov x8, x8
1655		mov x0, x8
1656		// statement primitives do not return results (but will clobber r0/rax)
1657		bl _prints
1658		bl _println
1659		// pop {x0}
1660		ldp x0, xzr, [sp], #16
1661	.L34:
1662		mov x0, #1
1663		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1664		mov sp, fp
1665		// pop {x19, x20}
1666		ldp x19, x20, [sp], #16
1667		// pop {fp, lr}
1668		ldp fp, lr, [sp], #16
1669		ret
1670		// Stack pointer unchanged, no stack allocated variables
1671		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1672	
1673	wacc_handleMenuCount:
1674		// push {fp, lr}
1675		stp fp, lr, [sp, #-16]!
1676		// push {x19}
1677		stp x19, xzr, [sp, #-16]!
1678		mov fp, sp
1679		// Stack pointer unchanged, no stack allocated variables
1680		// push {x0}
1681		stp x0, xzr, [sp, #-16]!
1682		// Set up X16 as a temporary second base pointer for the caller saved things
1683		mov x16, sp
1684		// Stack pointer unchanged, no stack allocated arguments
1685		mov x8, x0
1686		mov x0, x8
1687		bl wacc_count
1688		mov x16, x0
1689		// Stack pointer unchanged, no stack allocated arguments
1690		// pop {x0}
1691		ldp x0, xzr, [sp], #16
1692		mov x8, x16
1693		mov x19, x8
1694		cmp x19, #1
1695		b.eq .L35
1696		// push {x0}
1697		stp x0, xzr, [sp, #-16]!
1698		// Set up X16 as a temporary second base pointer for the caller saved things
1699		mov x16, sp
1700		// Stack pointer unchanged, no stack allocated arguments
1701		adrp x8, .L.str21
1702		add x8, x8, :lo12:.L.str21
1703		// push {x8}
1704		stp x8, xzr, [sp, #-16]!
1705		// pop {x8}
1706		ldp x8, xzr, [sp], #16
1707		mov x8, x8
1708		mov x0, x8
1709		// statement primitives do not return results (but will clobber r0/rax)
1710		bl _prints
1711		// pop {x0}
1712		ldp x0, xzr, [sp], #16
1713		// push {x0}
1714		stp x0, xzr, [sp, #-16]!
1715		// Set up X16 as a temporary second base pointer for the caller saved things
1716		mov x16, sp
1717		// Stack pointer unchanged, no stack allocated arguments
1718		mov x8, x19
1719		mov x0, x8
1720		// statement primitives do not return results (but will clobber r0/rax)
1721		bl _printi
1722		// pop {x0}
1723		ldp x0, xzr, [sp], #16
1724		// push {x0}
1725		stp x0, xzr, [sp, #-16]!
1726		// Set up X16 as a temporary second base pointer for the caller saved things
1727		mov x16, sp
1728		// Stack pointer unchanged, no stack allocated arguments
1729		adrp x8, .L.str22
1730		add x8, x8, :lo12:.L.str22
1731		// push {x8}
1732		stp x8, xzr, [sp, #-16]!
1733		// pop {x8}
1734		ldp x8, xzr, [sp], #16
1735		mov x8, x8
1736		mov x0, x8
1737		// statement primitives do not return results (but will clobber r0/rax)
1738		bl _prints
1739		bl _println
1740		// pop {x0}
1741		ldp x0, xzr, [sp], #16
1742		b .L36
1743	.L35:
1744		// push {x0}
1745		stp x0, xzr, [sp, #-16]!
1746		// Set up X16 as a temporary second base pointer for the caller saved things
1747		mov x16, sp
1748		// Stack pointer unchanged, no stack allocated arguments
1749		adrp x8, .L.str23
1750		add x8, x8, :lo12:.L.str23
1751		// push {x8}
1752		stp x8, xzr, [sp, #-16]!
1753		// pop {x8}
1754		ldp x8, xzr, [sp], #16
1755		mov x8, x8
1756		mov x0, x8
1757		// statement primitives do not return results (but will clobber r0/rax)
1758		bl _prints
1759		bl _println
1760		// pop {x0}
1761		ldp x0, xzr, [sp], #16
1762	.L36:
1763		mov x0, #1
1764		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1765		mov sp, fp
1766		// pop {x19}
1767		ldp x19, xzr, [sp], #16
1768		// pop {fp, lr}
1769		ldp fp, lr, [sp], #16
1770		ret
1771		// Stack pointer unchanged, no stack allocated variables
1772		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1773	
1774	wacc_handleMenuPrint:
1775		// push {fp, lr}
1776		stp fp, lr, [sp, #-16]!
1777		// push {x19}
1778		stp x19, xzr, [sp, #-16]!
1779		mov fp, sp
1780		// Stack pointer unchanged, no stack allocated variables
1781		// push {x0}
1782		stp x0, xzr, [sp, #-16]!
1783		// Set up X16 as a temporary second base pointer for the caller saved things
1784		mov x16, sp
1785		// Stack pointer unchanged, no stack allocated arguments
1786		adrp x8, .L.str24
1787		add x8, x8, :lo12:.L.str24
1788		// push {x8}
1789		stp x8, xzr, [sp, #-16]!
1790		// pop {x8}
1791		ldp x8, xzr, [sp], #16
1792		mov x8, x8
1793		mov x0, x8
1794		// statement primitives do not return results (but will clobber r0/rax)
1795		bl _prints
1796		// pop {x0}
1797		ldp x0, xzr, [sp], #16
1798		// push {x0}
1799		stp x0, xzr, [sp, #-16]!
1800		// Set up X16 as a temporary second base pointer for the caller saved things
1801		mov x16, sp
1802		// Stack pointer unchanged, no stack allocated arguments
1803		mov x8, x0
1804		mov x0, x8
1805		bl wacc_printAll
1806		mov x16, x0
1807		// Stack pointer unchanged, no stack allocated arguments
1808		// pop {x0}
1809		ldp x0, xzr, [sp], #16
1810		mov x8, x16
1811		mov x19, x8
1812		mov x0, #1
1813		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1814		mov sp, fp
1815		// pop {x19}
1816		ldp x19, xzr, [sp], #16
1817		// pop {fp, lr}
1818		ldp fp, lr, [sp], #16
1819		ret
1820		// Stack pointer unchanged, no stack allocated variables
1821		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1822	
1823	wacc_handleMenuRemove:
1824		// push {fp, lr}
1825		stp fp, lr, [sp, #-16]!
1826		// push {x19, x20}
1827		stp x19, x20, [sp, #-16]!
1828		mov fp, sp
1829		// Stack pointer unchanged, no stack allocated variables
1830		// push {x0}
1831		stp x0, xzr, [sp, #-16]!
1832		// Set up X16 as a temporary second base pointer for the caller saved things
1833		mov x16, sp
1834		// Stack pointer unchanged, no stack allocated arguments
1835		adrp x8, .L.str25
1836		add x8, x8, :lo12:.L.str25
1837		// push {x8}
1838		stp x8, xzr, [sp, #-16]!
1839		// pop {x8}
1840		ldp x8, xzr, [sp], #16
1841		mov x8, x8
1842		mov x0, x8
1843		bl wacc_askForInt
1844		mov x16, x0
1845		// Stack pointer unchanged, no stack allocated arguments
1846		// pop {x0}
1847		ldp x0, xzr, [sp], #16
1848		mov x8, x16
1849		mov x19, x8
1850		// push {x0}
1851		stp x0, xzr, [sp, #-16]!
1852		// Set up X16 as a temporary second base pointer for the caller saved things
1853		mov x16, sp
1854		// Stack pointer unchanged, no stack allocated arguments
1855		mov x8, x0
1856		mov x0, x8
1857		mov x8, x19
1858		mov x1, x8
1859		bl wacc_remove
1860		mov x16, x0
1861		// Stack pointer unchanged, no stack allocated arguments
1862		// pop {x0}
1863		ldp x0, xzr, [sp], #16
1864		mov x8, x16
1865		mov x20, x8
1866		cmp x20, #1
1867		b.eq .L37
1868		// push {x0}
1869		stp x0, xzr, [sp, #-16]!
1870		// Set up X16 as a temporary second base pointer for the caller saved things
1871		mov x16, sp
1872		// Stack pointer unchanged, no stack allocated arguments
1873		adrp x8, .L.str19
1874		add x8, x8, :lo12:.L.str19
1875		// push {x8}
1876		stp x8, xzr, [sp, #-16]!
1877		// pop {x8}
1878		ldp x8, xzr, [sp], #16
1879		mov x8, x8
1880		mov x0, x8
1881		// statement primitives do not return results (but will clobber r0/rax)
1882		bl _prints
1883		bl _println
1884		// pop {x0}
1885		ldp x0, xzr, [sp], #16
1886		b .L38
1887	.L37:
1888		// push {x0}
1889		stp x0, xzr, [sp, #-16]!
1890		// Set up X16 as a temporary second base pointer for the caller saved things
1891		mov x16, sp
1892		// Stack pointer unchanged, no stack allocated arguments
1893		adrp x8, .L.str26
1894		add x8, x8, :lo12:.L.str26
1895		// push {x8}
1896		stp x8, xzr, [sp, #-16]!
1897		// pop {x8}
1898		ldp x8, xzr, [sp], #16
1899		mov x8, x8
1900		mov x0, x8
1901		// statement primitives do not return results (but will clobber r0/rax)
1902		bl _prints
1903		bl _println
1904		// pop {x0}
1905		ldp x0, xzr, [sp], #16
1906	.L38:
1907		mov x0, #1
1908		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1909		mov sp, fp
1910		// pop {x19, x20}
1911		ldp x19, x20, [sp], #16
1912		// pop {fp, lr}
1913		ldp fp, lr, [sp], #16
1914		ret
1915		// Stack pointer unchanged, no stack allocated variables
1916		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1917	
1918	wacc_handleMenuRemoveAll:
1919		// push {fp, lr}
1920		stp fp, lr, [sp, #-16]!
1921		// push {x19}
1922		stp x19, xzr, [sp, #-16]!
1923		mov fp, sp
1924		// Stack pointer unchanged, no stack allocated variables
1925		// push {x0}
1926		stp x0, xzr, [sp, #-16]!
1927		// Set up X16 as a temporary second base pointer for the caller saved things
1928		mov x16, sp
1929		// Stack pointer unchanged, no stack allocated arguments
1930		mov x8, x0
1931		mov x0, x8
1932		bl wacc_removeAll
1933		mov x16, x0
1934		// Stack pointer unchanged, no stack allocated arguments
1935		// pop {x0}
1936		ldp x0, xzr, [sp], #16
1937		mov x8, x16
1938		mov x19, x8
1939		// push {x0}
1940		stp x0, xzr, [sp, #-16]!
1941		// Set up X16 as a temporary second base pointer for the caller saved things
1942		mov x16, sp
1943		// Stack pointer unchanged, no stack allocated arguments
1944		adrp x8, .L.str27
1945		add x8, x8, :lo12:.L.str27
1946		// push {x8}
1947		stp x8, xzr, [sp, #-16]!
1948		// pop {x8}
1949		ldp x8, xzr, [sp], #16
1950		mov x8, x8
1951		mov x0, x8
1952		// statement primitives do not return results (but will clobber r0/rax)
1953		bl _prints
1954		bl _println
1955		// pop {x0}
1956		ldp x0, xzr, [sp], #16
1957		mov x0, #1
1958		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1959		mov sp, fp
1960		// pop {x19}
1961		ldp x19, xzr, [sp], #16
1962		// pop {fp, lr}
1963		ldp fp, lr, [sp], #16
1964		ret
1965		// Stack pointer unchanged, no stack allocated variables
1966		// 'ere be dragons: this is 100% dead code, functions always end in returns!
1967	
1968	_freepair:
1969		// push {lr}
1970		stp lr, xzr, [sp, #-16]!
1971		cbz x0, _errNull
1972		bl free
1973		// pop {lr}
1974		ldp lr, xzr, [sp], #16
1975		ret
1976	
1977	// length of .L._readc_str0
1978		.word 3
1979	.L._readc_str0:
1980		.asciz " %c"
1981	.align 4
1982	_readc:
1983		// X0 contains the "original" value of the destination of the read
1984		// allocate space on the stack to store the read: preserve alignment!
1985		// the passed default argument should be stored in case of EOF
1986		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
1987		// push {x0, lr}
1988		stp x0, lr, [sp, #-16]!
1989		mov x1, sp
1990		adr x0, .L._readc_str0
1991		bl scanf
1992		// pop {x0, lr}
1993		ldp x0, lr, [sp], #16
1994		ret
1995	
1996	// length of .L._prints_str0
1997		.word 4
1998	.L._prints_str0:
1999		.asciz "%.*s"
2000	.align 4
2001	_prints:
2002		// push {lr}
2003		stp lr, xzr, [sp, #-16]!
2004		mov x2, x0
2005		ldrsw x1, [x0, #-4]
2006		adr x0, .L._prints_str0
2007		bl printf
2008		mov x0, #0
2009		bl fflush
2010		// pop {lr}
2011		ldp lr, xzr, [sp], #16
2012		ret
2013	
2014	// length of .L._readi_str0
2015		.word 2
2016	.L._readi_str0:
2017		.asciz "%d"
2018	.align 4
2019	_readi:
2020		// X0 contains the "original" value of the destination of the read
2021		// allocate space on the stack to store the read: preserve alignment!
2022		// the passed default argument should be stored in case of EOF
2023		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
2024		// push {x0, lr}
2025		stp x0, lr, [sp, #-16]!
2026		mov x1, sp
2027		adr x0, .L._readi_str0
2028		bl scanf
2029		// pop {x0, lr}
2030		ldp x0, lr, [sp], #16
2031		ret
2032	
2033	_malloc:
2034		// push {lr}
2035		stp lr, xzr, [sp, #-16]!
2036		bl malloc
2037		cbz x0, _errOutOfMemory
2038		// pop {lr}
2039		ldp lr, xzr, [sp], #16
2040		ret
2041	
2042	// length of .L._printi_str0
2043		.word 2
2044	.L._printi_str0:
2045		.asciz "%d"
2046	.align 4
2047	_printi:
2048		// push {lr}
2049		stp lr, xzr, [sp, #-16]!
2050		mov x1, x0
2051		adr x0, .L._printi_str0
2052		bl printf
2053		mov x0, #0
2054		bl fflush
2055		// pop {lr}
2056		ldp lr, xzr, [sp], #16
2057		ret
2058	
2059	// length of .L._printc_str0
2060		.word 2
2061	.L._printc_str0:
2062		.asciz "%c"
2063	.align 4
2064	_printc:
2065		// push {lr}
2066		stp lr, xzr, [sp, #-16]!
2067		mov x1, x0
2068		adr x0, .L._printc_str0
2069		bl printf
2070		mov x0, #0
2071		bl fflush
2072		// pop {lr}
2073		ldp lr, xzr, [sp], #16
2074		ret
2075	
2076	// length of .L._println_str0
2077		.word 0
2078	.L._println_str0:
2079		.asciz ""
2080	.align 4
2081	_println:
2082		// push {lr}
2083		stp lr, xzr, [sp, #-16]!
2084		adr x0, .L._println_str0
2085		bl puts
2086		mov x0, #0
2087		bl fflush
2088		// pop {lr}
2089		ldp lr, xzr, [sp], #16
2090		ret
2091	
2092	_arrLoad8:
2093		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
2094		// push {lr}
2095		stp lr, xzr, [sp, #-16]!
2096		sxtw x17, w17
2097		cmp w17, #0
2098		csel x1, x17, x1, lt
2099		b.lt _errOutOfBounds
2100		ldrsw lr, [x7, #-4]
2101		cmp w17, w30
2102		csel x1, x17, x1, ge
2103		b.ge _errOutOfBounds
2104		ldr x7, [x7, x17, lsl #3]
2105		// pop {lr}
2106		ldp lr, xzr, [sp], #16
2107		ret
2108	
2109	_arrStore8:
2110		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
2111		// push {lr}
2112		stp lr, xzr, [sp, #-16]!
2113		sxtw x17, w17
2114		cmp w17, #0
2115		csel x1, x17, x1, lt
2116		b.lt _errOutOfBounds
2117		ldrsw lr, [x7, #-4]
2118		cmp w17, w30
2119		csel x1, x17, x1, ge
2120		b.ge _errOutOfBounds
2121		str x8, [x7, x17, lsl #3]
2122		// pop {lr}
2123		ldp lr, xzr, [sp], #16
2124		ret
2125	
2126	// length of .L._errOutOfMemory_str0
2127		.word 27
2128	.L._errOutOfMemory_str0:
2129		.asciz "fatal error: out of memory\n"
2130	.align 4
2131	_errOutOfMemory:
2132		adr x0, .L._errOutOfMemory_str0
2133		bl _prints
2134		mov w0, #-1
2135		bl exit
2136	
2137	// length of .L._errDivZero_str0
2138		.word 40
2139	.L._errDivZero_str0:
2140		.asciz "fatal error: division or modulo by zero\n"
2141	.align 4
2142	_errDivZero:
2143		adr x0, .L._errDivZero_str0
2144		bl _prints
2145		mov w0, #-1
2146		bl exit
2147	
2148	// length of .L._errNull_str0
2149		.word 45
2150	.L._errNull_str0:
2151		.asciz "fatal error: null pair dereferenced or freed\n"
2152	.align 4
2153	_errNull:
2154		adr x0, .L._errNull_str0
2155		bl _prints
2156		mov w0, #-1
2157		bl exit
2158	
2159	// length of .L._errOutOfBounds_str0
2160		.word 42
2161	.L._errOutOfBounds_str0:
2162		.asciz "fatal error: array index %d out of bounds\n"
2163	.align 4
2164	_errOutOfBounds:
2165		adr x0, .L._errOutOfBounds_str0
2166		bl printf
2167		mov x0, #0
2168		bl fflush
2169		mov w0, #-1
2170		bl exit
2171	
2172	// length of .L._errOverflow_str0
2173		.word 52
2174	.L._errOverflow_str0:
2175		.asciz "fatal error: integer overflow or underflow occurred\n"
2176	.align 4
2177	_errOverflow:
2178		adr x0, .L._errOverflow_str0
2179		bl _prints
2180		mov w0, #-1
2181		bl exit
===========================================================
-- Finished

