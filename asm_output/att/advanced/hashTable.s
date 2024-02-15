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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 0
4	.L.str0:
5		.asciz ""
6	# length of .L.str1
7		.int 43
8	.L.str1:
9		.asciz "==========================================="
10	# length of .L.str2
11		.int 43
12	.L.str2:
13		.asciz "========== Hash Table Program ============="
14	# length of .L.str3
15		.int 43
16	.L.str3:
17		.asciz "=                                         ="
18	# length of .L.str4
19		.int 43
20	.L.str4:
21		.asciz "= Please choose the following options:    ="
22	# length of .L.str5
23		.int 43
24	.L.str5:
25		.asciz "= a: insert an integer                    ="
26	# length of .L.str6
27		.int 43
28	.L.str6:
29		.asciz "= b: find an integer                      ="
30	# length of .L.str7
31		.int 43
32	.L.str7:
33		.asciz "= c: count the integers                   ="
34	# length of .L.str8
35		.int 43
36	.L.str8:
37		.asciz "= d: print all integers                   ="
38	# length of .L.str9
39		.int 43
40	.L.str9:
41		.asciz "= e: remove an integer                    ="
42	# length of .L.str10
43		.int 43
44	.L.str10:
45		.asciz "= f: remove all integers                  ="
46	# length of .L.str11
47		.int 43
48	.L.str11:
49		.asciz "= g: exit                                 ="
50	# length of .L.str12
51		.int 15
52	.L.str12:
53		.asciz "Your decision: "
54	# length of .L.str13
55		.int 18
56	.L.str13:
57		.asciz "You have entered: "
58	# length of .L.str14
59		.int 36
60	.L.str14:
61		.asciz " which is invalid, please try again."
62	# length of .L.str15
63		.int 35
64	.L.str15:
65		.asciz "Please enter an integer to insert: "
66	# length of .L.str16
67		.int 51
68	.L.str16:
69		.asciz "The integer is already there. No insertion is made."
70	# length of .L.str17
71		.int 43
72	.L.str17:
73		.asciz "Successfully insert it. The integer is new."
74	# length of .L.str18
75		.int 33
76	.L.str18:
77		.asciz "Please enter an integer to find: "
78	# length of .L.str19
79		.int 25
80	.L.str19:
81		.asciz "The integer is not found."
82	# length of .L.str20
83		.int 17
84	.L.str20:
85		.asciz "Find the integer."
86	# length of .L.str21
87		.int 10
88	.L.str21:
89		.asciz "There are "
90	# length of .L.str22
91		.int 10
92	.L.str22:
93		.asciz " integers."
94	# length of .L.str23
95		.int 24
96	.L.str23:
97		.asciz "There is only 1 integer."
98	# length of .L.str24
99		.int 23
100	.L.str24:
101		.asciz "Here are the integers: "
102	# length of .L.str25
103		.int 35
104	.L.str25:
105		.asciz "Please enter an integer to remove: "
106	# length of .L.str26
107		.int 29
108	.L.str26:
109		.asciz "The integer has been removed."
110	# length of .L.str27
111		.int 31
112	.L.str27:
113		.asciz "All integers have been removed."
114	# length of .L.str28
115		.int 23
116	.L.str28:
117		.asciz "Error: unknown choice ("
118	# length of .L.str29
119		.int 1
120	.L.str29:
121		.asciz ")"
122	# length of .L.str30
123		.int 13
124	.L.str30:
125		.asciz "Goodbye Human"
126	.text
127	main:
128		pushq %rbp
129		# pushq {%rbx, %r12, %r13, %r14, %r15}
130		subq $40, %rsp
131		movq %rbx, (%rsp)
132		movq %r12, 8(%rsp)
133		movq %r13, 16(%rsp)
134		movq %r14, 24(%rsp)
135		movq %r15, 32(%rsp)
136		movq %rsp, %rbp
137		# Stack pointer unchanged, no stack allocated variables
138		# 13 element array
139		pushq %rcx
140		movl $108, %edi
141		call _malloc
142		movq %rax, %r11
143		popq %rcx
144		# array pointers are shifted forwards by 4 bytes (to account for size)
145		movq %r11, %r11
146		addq $4, %r11
147		movq $13, %rax
148		movl %eax, -4(%r11)
149		movq $0, %rax
150		movq %rax, (%r11)
151		movq $0, %rax
152		movq %rax, 8(%r11)
153		movq $0, %rax
154		movq %rax, 16(%r11)
155		movq $0, %rax
156		movq %rax, 24(%r11)
157		movq $0, %rax
158		movq %rax, 32(%r11)
159		movq $0, %rax
160		movq %rax, 40(%r11)
161		movq $0, %rax
162		movq %rax, 48(%r11)
163		movq $0, %rax
164		movq %rax, 56(%r11)
165		movq $0, %rax
166		movq %rax, 64(%r11)
167		movq $0, %rax
168		movq %rax, 72(%r11)
169		movq $0, %rax
170		movq %rax, 80(%r11)
171		movq $0, %rax
172		movq %rax, 88(%r11)
173		movq $0, %rax
174		movq %rax, 96(%r11)
175		movq %r11, %rax
176		movq %rax, %r12
177		pushq %rcx
178		# Set up R11 as a temporary second base pointer for the caller saved things
179		movq %rsp, %r11
180		# Stack pointer unchanged, no stack allocated arguments
181		movq %r12, %rax
182		movq %rax, %rdi
183		call wacc_init
184		movq %rax, %r11
185		# Stack pointer unchanged, no stack allocated arguments
186		popq %rcx
187		movq %r11, %rax
188		movq %rax, %r13
189		movq $1, %rax
190		movq %rax, %r14
191		jmp .L39
192	.L40:
193		# Stack pointer unchanged, no stack allocated variables
194		pushq %rcx
195		# Set up R11 as a temporary second base pointer for the caller saved things
196		movq %rsp, %r11
197		# Stack pointer unchanged, no stack allocated arguments
198		call wacc_printMenu
199		movq %rax, %r11
200		# Stack pointer unchanged, no stack allocated arguments
201		popq %rcx
202		movq %r11, %rax
203		movq %rax, %r15
204		cmpq $97, %r15
205		je .L41
206		cmpq $98, %r15
207		je .L43
208		cmpq $99, %r15
209		je .L45
210		cmpq $100, %r15
211		je .L47
212		cmpq $101, %r15
213		je .L49
214		cmpq $102, %r15
215		je .L51
216		cmpq $103, %r15
217		je .L53
218		pushq %rcx
219		# Set up R11 as a temporary second base pointer for the caller saved things
220		movq %rsp, %r11
221		# Stack pointer unchanged, no stack allocated arguments
222		leaq .L.str28(%rip), %rax
223		pushq %rax
224		popq %rax
225		movq %rax, %rax
226		movq %rax, %rdi
227		# statement primitives do not return results (but will clobber r0/rax)
228		call _prints
229		popq %rcx
230		pushq %rcx
231		# Set up R11 as a temporary second base pointer for the caller saved things
232		movq %rsp, %r11
233		# Stack pointer unchanged, no stack allocated arguments
234		movq %r15, %rax
235		movq %rax, %rdi
236		# statement primitives do not return results (but will clobber r0/rax)
237		call _printc
238		popq %rcx
239		pushq %rcx
240		# Set up R11 as a temporary second base pointer for the caller saved things
241		movq %rsp, %r11
242		# Stack pointer unchanged, no stack allocated arguments
243		leaq .L.str29(%rip), %rax
244		pushq %rax
245		popq %rax
246		movq %rax, %rax
247		movq %rax, %rdi
248		# statement primitives do not return results (but will clobber r0/rax)
249		call _prints
250		call _println
251		popq %rcx
252		pushq %rcx
253		# Set up R11 as a temporary second base pointer for the caller saved things
254		movq %rsp, %r11
255		# Stack pointer unchanged, no stack allocated arguments
256		movq $-1, %rax
257		movq %rax, %rdi
258		# statement primitives do not return results (but will clobber r0/rax)
259		call _exit
260		popq %rcx
261		jmp .L54
262	.L53:
263		pushq %rcx
264		# Set up R11 as a temporary second base pointer for the caller saved things
265		movq %rsp, %r11
266		# Stack pointer unchanged, no stack allocated arguments
267		leaq .L.str30(%rip), %rax
268		pushq %rax
269		popq %rax
270		movq %rax, %rax
271		movq %rax, %rdi
272		# statement primitives do not return results (but will clobber r0/rax)
273		call _prints
274		call _println
275		popq %rcx
276		movq $0, %rax
277		movq %rax, %r14
278	.L54:
279		jmp .L52
280	.L51:
281		# Stack pointer unchanged, no stack allocated variables
282		pushq %rcx
283		# Set up R11 as a temporary second base pointer for the caller saved things
284		movq %rsp, %r11
285		# Stack pointer unchanged, no stack allocated arguments
286		movq %r12, %rax
287		movq %rax, %rdi
288		call wacc_handleMenuRemoveAll
289		movq %rax, %r11
290		# Stack pointer unchanged, no stack allocated arguments
291		popq %rcx
292		movq %r11, %rax
293		movq %rax, %rcx
294		# Stack pointer unchanged, no stack allocated variables
295	.L52:
296		jmp .L50
297	.L49:
298		# Stack pointer unchanged, no stack allocated variables
299		pushq %rcx
300		# Set up R11 as a temporary second base pointer for the caller saved things
301		movq %rsp, %r11
302		# Stack pointer unchanged, no stack allocated arguments
303		movq %r12, %rax
304		movq %rax, %rdi
305		call wacc_handleMenuRemove
306		movq %rax, %r11
307		# Stack pointer unchanged, no stack allocated arguments
308		popq %rcx
309		movq %r11, %rax
310		movq %rax, %rcx
311		# Stack pointer unchanged, no stack allocated variables
312	.L50:
313		jmp .L48
314	.L47:
315		# Stack pointer unchanged, no stack allocated variables
316		pushq %rcx
317		# Set up R11 as a temporary second base pointer for the caller saved things
318		movq %rsp, %r11
319		# Stack pointer unchanged, no stack allocated arguments
320		movq %r12, %rax
321		movq %rax, %rdi
322		call wacc_handleMenuPrint
323		movq %rax, %r11
324		# Stack pointer unchanged, no stack allocated arguments
325		popq %rcx
326		movq %r11, %rax
327		movq %rax, %rcx
328		# Stack pointer unchanged, no stack allocated variables
329	.L48:
330		jmp .L46
331	.L45:
332		# Stack pointer unchanged, no stack allocated variables
333		pushq %rcx
334		# Set up R11 as a temporary second base pointer for the caller saved things
335		movq %rsp, %r11
336		# Stack pointer unchanged, no stack allocated arguments
337		movq %r12, %rax
338		movq %rax, %rdi
339		call wacc_handleMenuCount
340		movq %rax, %r11
341		# Stack pointer unchanged, no stack allocated arguments
342		popq %rcx
343		movq %r11, %rax
344		movq %rax, %rcx
345		# Stack pointer unchanged, no stack allocated variables
346	.L46:
347		jmp .L44
348	.L43:
349		# Stack pointer unchanged, no stack allocated variables
350		pushq %rcx
351		# Set up R11 as a temporary second base pointer for the caller saved things
352		movq %rsp, %r11
353		# Stack pointer unchanged, no stack allocated arguments
354		movq %r12, %rax
355		movq %rax, %rdi
356		call wacc_handleMenuFind
357		movq %rax, %r11
358		# Stack pointer unchanged, no stack allocated arguments
359		popq %rcx
360		movq %r11, %rax
361		movq %rax, %rcx
362		# Stack pointer unchanged, no stack allocated variables
363	.L44:
364		jmp .L42
365	.L41:
366		# Stack pointer unchanged, no stack allocated variables
367		pushq %rcx
368		# Set up R11 as a temporary second base pointer for the caller saved things
369		movq %rsp, %r11
370		# Stack pointer unchanged, no stack allocated arguments
371		movq %r12, %rax
372		movq %rax, %rdi
373		call wacc_handleMenuInsert
374		movq %rax, %r11
375		# Stack pointer unchanged, no stack allocated arguments
376		popq %rcx
377		movq %r11, %rax
378		movq %rax, %rcx
379		# Stack pointer unchanged, no stack allocated variables
380	.L42:
381		# Stack pointer unchanged, no stack allocated variables
382	.L39:
383		cmpq $1, %r14
384		je .L40
385		# Stack pointer unchanged, no stack allocated variables
386		movq $0, %rax
387		# popq {%rbx, %r12, %r13, %r14, %r15}
388		movq (%rsp), %rbx
389		movq 8(%rsp), %r12
390		movq 16(%rsp), %r13
391		movq 24(%rsp), %r14
392		movq 32(%rsp), %r15
393		addq $40, %rsp
394		popq %rbp
395		ret
396	
397	wacc_init:
398		pushq %rbp
399		# pushq {%r12, %r13}
400		subq $16, %rsp
401		movq %r12, (%rsp)
402		movq %r13, 8(%rsp)
403		movq %rsp, %rbp
404		# Stack pointer unchanged, no stack allocated variables
405		movslq -4(%rdi), %rax
406		movq %rax, %rax
407		movq %rax, %r12
408		movq $0, %rax
409		movq %rax, %r13
410		jmp .L0
411	.L1:
412		movl %r13d, %r10d
413		movq $0, %rax
414		movq %rdi, %r9
415		call _arrStore8
416		movl %r13d, %eax
417		addl $1, %eax
418		jo _errOverflow
419		movslq %eax, %rax
420		pushq %rax
421		popq %rax
422		movq %rax, %rax
423		movq %rax, %r13
424	.L0:
425		cmpq %r12, %r13
426		jl .L1
427		movq $1, %rax
428		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
429		movq %rbp, %rsp
430		# popq {%r12, %r13}
431		movq (%rsp), %r12
432		movq 8(%rsp), %r13
433		addq $16, %rsp
434		popq %rbp
435		ret
436		# Stack pointer unchanged, no stack allocated variables
437		# 'ere be dragons: this is 100% dead code, functions always end in returns!
438	
439	wacc_contain:
440		pushq %rbp
441		# pushq {%r12, %r13}
442		subq $16, %rsp
443		movq %r12, (%rsp)
444		movq %r13, 8(%rsp)
445		movq %rsp, %rbp
446		# Stack pointer unchanged, no stack allocated variables
447		# pushq {%rsi, %rdi}
448		subq $16, %rsp
449		movq %rsi, (%rsp)
450		movq %rdi, 8(%rsp)
451		# Set up R11 as a temporary second base pointer for the caller saved things
452		movq %rsp, %r11
453		# Stack pointer unchanged, no stack allocated arguments
454		movq %rdi, %rax
455		movq %rax, %rdi
456		movq %rsi, %rax
457		movq %rax, %rsi
458		call wacc_calculateIndex
459		movq %rax, %r11
460		# Stack pointer unchanged, no stack allocated arguments
461		# popq {%rsi, %rdi}
462		movq (%rsp), %rsi
463		movq 8(%rsp), %rdi
464		addq $16, %rsp
465		movq %r11, %rax
466		movq %rax, %r12
467		# pushq {%rsi, %rdi}
468		subq $16, %rsp
469		movq %rsi, (%rsp)
470		movq %rdi, 8(%rsp)
471		# Set up R11 as a temporary second base pointer for the caller saved things
472		movq %rsp, %r11
473		# Stack pointer unchanged, no stack allocated arguments
474		movl %r12d, %r10d
475		movq %rdi, %r9
476		call _arrLoad8
477		movq %r9, %rax
478		movq %rax, %rax
479		movq %rax, %rdi
480		movq %rsi, %rax
481		movq %rax, %rsi
482		call wacc_findNode
483		movq %rax, %r11
484		# Stack pointer unchanged, no stack allocated arguments
485		# popq {%rsi, %rdi}
486		movq (%rsp), %rsi
487		movq 8(%rsp), %rdi
488		addq $16, %rsp
489		movq %r11, %rax
490		movq %rax, %r13
491		cmpq $0, %r13
492		setne %al
493		movsbq %al, %rax
494		pushq %rax
495		popq %rax
496		movq %rax, %rax
497		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
498		movq %rbp, %rsp
499		# popq {%r12, %r13}
500		movq (%rsp), %r12
501		movq 8(%rsp), %r13
502		addq $16, %rsp
503		popq %rbp
504		ret
505		# Stack pointer unchanged, no stack allocated variables
506		# 'ere be dragons: this is 100% dead code, functions always end in returns!
507	
508	wacc_insertIfNotContain:
509		pushq %rbp
510		# pushq {%r12, %r13, %r14}
511		subq $24, %rsp
512		movq %r12, (%rsp)
513		movq %r13, 8(%rsp)
514		movq %r14, 16(%rsp)
515		movq %rsp, %rbp
516		# Stack pointer unchanged, no stack allocated variables
517		# pushq {%rsi, %rdi}
518		subq $16, %rsp
519		movq %rsi, (%rsp)
520		movq %rdi, 8(%rsp)
521		# Set up R11 as a temporary second base pointer for the caller saved things
522		movq %rsp, %r11
523		# Stack pointer unchanged, no stack allocated arguments
524		movq %rdi, %rax
525		movq %rax, %rdi
526		movq %rsi, %rax
527		movq %rax, %rsi
528		call wacc_calculateIndex
529		movq %rax, %r11
530		# Stack pointer unchanged, no stack allocated arguments
531		# popq {%rsi, %rdi}
532		movq (%rsp), %rsi
533		movq 8(%rsp), %rdi
534		addq $16, %rsp
535		movq %r11, %rax
536		movq %rax, %r12
537		# pushq {%rsi, %rdi}
538		subq $16, %rsp
539		movq %rsi, (%rsp)
540		movq %rdi, 8(%rsp)
541		# Set up R11 as a temporary second base pointer for the caller saved things
542		movq %rsp, %r11
543		# Stack pointer unchanged, no stack allocated arguments
544		movl %r12d, %r10d
545		movq %rdi, %r9
546		call _arrLoad8
547		movq %r9, %rax
548		movq %rax, %rax
549		movq %rax, %rdi
550		movq %rsi, %rax
551		movq %rax, %rsi
552		call wacc_findNode
553		movq %rax, %r11
554		# Stack pointer unchanged, no stack allocated arguments
555		# popq {%rsi, %rdi}
556		movq (%rsp), %rsi
557		movq 8(%rsp), %rdi
558		addq $16, %rsp
559		movq %r11, %rax
560		movq %rax, %r13
561		cmpq $0, %r13
562		jne .L2
563		# Stack pointer unchanged, no stack allocated variables
564		# pushq {%rsi, %rdi}
565		subq $16, %rsp
566		movq %rsi, (%rsp)
567		movq %rdi, 8(%rsp)
568		movl $16, %edi
569		call _malloc
570		movq %rax, %r11
571		# popq {%rsi, %rdi}
572		movq (%rsp), %rsi
573		movq 8(%rsp), %rdi
574		addq $16, %rsp
575		movq %rsi, %rax
576		movq %rax, (%r11)
577		movl %r12d, %r10d
578		movq %rdi, %r9
579		call _arrLoad8
580		movq %r9, %rax
581		movq %rax, %rax
582		movq %rax, 8(%r11)
583		movq %r11, %rax
584		movq %rax, %r14
585		movl %r12d, %r10d
586		movq %r14, %rax
587		movq %rdi, %r9
588		call _arrStore8
589		movq $1, %rax
590		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
591		movq %rbp, %rsp
592		# popq {%r12, %r13, %r14}
593		movq (%rsp), %r12
594		movq 8(%rsp), %r13
595		movq 16(%rsp), %r14
596		addq $24, %rsp
597		popq %rbp
598		ret
599		# Stack pointer unchanged, no stack allocated variables
600		jmp .L3
601	.L2:
602		movq $0, %rax
603		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
604		movq %rbp, %rsp
605		# popq {%r12, %r13, %r14}
606		movq (%rsp), %r12
607		movq 8(%rsp), %r13
608		movq 16(%rsp), %r14
609		addq $24, %rsp
610		popq %rbp
611		ret
612	.L3:
613		# Stack pointer unchanged, no stack allocated variables
614		# 'ere be dragons: this is 100% dead code, functions always end in returns!
615	
616	wacc_remove:
617		pushq %rbp
618		# pushq {%r12, %r13}
619		subq $16, %rsp
620		movq %r12, (%rsp)
621		movq %r13, 8(%rsp)
622		movq %rsp, %rbp
623		# Stack pointer unchanged, no stack allocated variables
624		# pushq {%rsi, %rdi}
625		subq $16, %rsp
626		movq %rsi, (%rsp)
627		movq %rdi, 8(%rsp)
628		# Set up R11 as a temporary second base pointer for the caller saved things
629		movq %rsp, %r11
630		# Stack pointer unchanged, no stack allocated arguments
631		movq %rdi, %rax
632		movq %rax, %rdi
633		movq %rsi, %rax
634		movq %rax, %rsi
635		call wacc_calculateIndex
636		movq %rax, %r11
637		# Stack pointer unchanged, no stack allocated arguments
638		# popq {%rsi, %rdi}
639		movq (%rsp), %rsi
640		movq 8(%rsp), %rdi
641		addq $16, %rsp
642		movq %r11, %rax
643		movq %rax, %r12
644		# pushq {%rsi, %rdi}
645		subq $16, %rsp
646		movq %rsi, (%rsp)
647		movq %rdi, 8(%rsp)
648		# Set up R11 as a temporary second base pointer for the caller saved things
649		movq %rsp, %r11
650		# Stack pointer unchanged, no stack allocated arguments
651		movl %r12d, %r10d
652		movq %rdi, %r9
653		call _arrLoad8
654		movq %r9, %rax
655		movq %rax, %rax
656		movq %rax, %rdi
657		movq %rsi, %rax
658		movq %rax, %rsi
659		call wacc_findNode
660		movq %rax, %r11
661		# Stack pointer unchanged, no stack allocated arguments
662		# popq {%rsi, %rdi}
663		movq (%rsp), %rsi
664		movq 8(%rsp), %rdi
665		addq $16, %rsp
666		movq %r11, %rax
667		movq %rax, %r13
668		cmpq $0, %r13
669		je .L4
670		# pushq {%rsi, %rdi}
671		subq $16, %rsp
672		movq %rsi, (%rsp)
673		movq %rdi, 8(%rsp)
674		# Set up R11 as a temporary second base pointer for the caller saved things
675		movq %rsp, %r11
676		# Stack pointer unchanged, no stack allocated arguments
677		movl %r12d, %r10d
678		movq %rdi, %r9
679		call _arrLoad8
680		movq %r9, %rax
681		movq %rax, %rax
682		movq %rax, %rdi
683		movq %r13, %rax
684		movq %rax, %rsi
685		call wacc_removeNode
686		movq %rax, %r11
687		# Stack pointer unchanged, no stack allocated arguments
688		# popq {%rsi, %rdi}
689		movq (%rsp), %rsi
690		movq 8(%rsp), %rdi
691		addq $16, %rsp
692		movl %r12d, %r10d
693		movq %r11, %rax
694		movq %rdi, %r9
695		call _arrStore8
696		movq $1, %rax
697		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
698		movq %rbp, %rsp
699		# popq {%r12, %r13}
700		movq (%rsp), %r12
701		movq 8(%rsp), %r13
702		addq $16, %rsp
703		popq %rbp
704		ret
705		jmp .L5
706	.L4:
707		movq $0, %rax
708		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
709		movq %rbp, %rsp
710		# popq {%r12, %r13}
711		movq (%rsp), %r12
712		movq 8(%rsp), %r13
713		addq $16, %rsp
714		popq %rbp
715		ret
716	.L5:
717		# Stack pointer unchanged, no stack allocated variables
718		# 'ere be dragons: this is 100% dead code, functions always end in returns!
719	
720	wacc_removeAll:
721		pushq %rbp
722		# pushq {%r12, %r13, %r14, %r15}
723		subq $32, %rsp
724		movq %r12, (%rsp)
725		movq %r13, 8(%rsp)
726		movq %r14, 16(%rsp)
727		movq %r15, 24(%rsp)
728		movq %rsp, %rbp
729		# Stack pointer unchanged, no stack allocated variables
730		movslq -4(%rdi), %rax
731		movq %rax, %rax
732		movq %rax, %r12
733		movq $0, %rax
734		movq %rax, %r13
735		jmp .L6
736	.L7:
737		# Stack pointer unchanged, no stack allocated variables
738		movl %r13d, %r10d
739		movq %rdi, %r9
740		call _arrLoad8
741		movq %r9, %rax
742		movq %rax, %rax
743		movq %rax, %r14
744		jmp .L8
745	.L9:
746		# Stack pointer unchanged, no stack allocated variables
747		cmpq $0, %r14
748		je _errNull
749		movq 8(%r14), %rax
750		movq %rax, %rax
751		movq %rax, %r15
752		pushq %rdi
753		# Set up R11 as a temporary second base pointer for the caller saved things
754		movq %rsp, %r11
755		# Stack pointer unchanged, no stack allocated arguments
756		movq %r14, %rax
757		movq %rax, %rdi
758		# statement primitives do not return results (but will clobber r0/rax)
759		call _freepair
760		popq %rdi
761		movq %r15, %rax
762		movq %rax, %r14
763		# Stack pointer unchanged, no stack allocated variables
764	.L8:
765		cmpq $0, %r14
766		jne .L9
767		movl %r13d, %r10d
768		movq $0, %rax
769		movq %rdi, %r9
770		call _arrStore8
771		movl %r13d, %eax
772		addl $1, %eax
773		jo _errOverflow
774		movslq %eax, %rax
775		pushq %rax
776		popq %rax
777		movq %rax, %rax
778		movq %rax, %r13
779		# Stack pointer unchanged, no stack allocated variables
780	.L6:
781		cmpq %r12, %r13
782		jl .L7
783		movq $1, %rax
784		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
785		movq %rbp, %rsp
786		# popq {%r12, %r13, %r14, %r15}
787		movq (%rsp), %r12
788		movq 8(%rsp), %r13
789		movq 16(%rsp), %r14
790		movq 24(%rsp), %r15
791		addq $32, %rsp
792		popq %rbp
793		ret
794		# Stack pointer unchanged, no stack allocated variables
795		# 'ere be dragons: this is 100% dead code, functions always end in returns!
796	
797	wacc_count:
798		pushq %rbp
799		# pushq {%r12, %r13, %r14, %r15}
800		subq $32, %rsp
801		movq %r12, (%rsp)
802		movq %r13, 8(%rsp)
803		movq %r14, 16(%rsp)
804		movq %r15, 24(%rsp)
805		movq %rsp, %rbp
806		# Stack pointer unchanged, no stack allocated variables
807		movslq -4(%rdi), %rax
808		movq %rax, %rax
809		movq %rax, %r12
810		movq $0, %rax
811		movq %rax, %r13
812		movq $0, %rax
813		movq %rax, %r14
814		jmp .L10
815	.L11:
816		# Stack pointer unchanged, no stack allocated variables
817		pushq %rdi
818		# Set up R11 as a temporary second base pointer for the caller saved things
819		movq %rsp, %r11
820		# Stack pointer unchanged, no stack allocated arguments
821		movl %r14d, %r10d
822		movq %rdi, %r9
823		call _arrLoad8
824		movq %r9, %rax
825		movq %rax, %rax
826		movq %rax, %rdi
827		call wacc_countNodes
828		movq %rax, %r11
829		# Stack pointer unchanged, no stack allocated arguments
830		popq %rdi
831		movq %r11, %rax
832		movq %rax, %r15
833		movl %r13d, %eax
834		addl %r15d, %eax
835		jo _errOverflow
836		movslq %eax, %rax
837		pushq %rax
838		popq %rax
839		movq %rax, %rax
840		movq %rax, %r13
841		movl %r14d, %eax
842		addl $1, %eax
843		jo _errOverflow
844		movslq %eax, %rax
845		pushq %rax
846		popq %rax
847		movq %rax, %rax
848		movq %rax, %r14
849		# Stack pointer unchanged, no stack allocated variables
850	.L10:
851		cmpq %r12, %r14
852		jl .L11
853		movq %r13, %rax
854		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
855		movq %rbp, %rsp
856		# popq {%r12, %r13, %r14, %r15}
857		movq (%rsp), %r12
858		movq 8(%rsp), %r13
859		movq 16(%rsp), %r14
860		movq 24(%rsp), %r15
861		addq $32, %rsp
862		popq %rbp
863		ret
864		# Stack pointer unchanged, no stack allocated variables
865		# 'ere be dragons: this is 100% dead code, functions always end in returns!
866	
867	wacc_printAll:
868		pushq %rbp
869		# pushq {%r12, %r13, %r14}
870		subq $24, %rsp
871		movq %r12, (%rsp)
872		movq %r13, 8(%rsp)
873		movq %r14, 16(%rsp)
874		movq %rsp, %rbp
875		# Stack pointer unchanged, no stack allocated variables
876		movslq -4(%rdi), %rax
877		movq %rax, %rax
878		movq %rax, %r12
879		movq $0, %rax
880		movq %rax, %r13
881		jmp .L12
882	.L13:
883		# Stack pointer unchanged, no stack allocated variables
884		pushq %rdi
885		# Set up R11 as a temporary second base pointer for the caller saved things
886		movq %rsp, %r11
887		# Stack pointer unchanged, no stack allocated arguments
888		movl %r13d, %r10d
889		movq %rdi, %r9
890		call _arrLoad8
891		movq %r9, %rax
892		movq %rax, %rax
893		movq %rax, %rdi
894		call wacc_printAllNodes
895		movq %rax, %r11
896		# Stack pointer unchanged, no stack allocated arguments
897		popq %rdi
898		movq %r11, %rax
899		movq %rax, %r14
900		movl %r13d, %eax
901		addl $1, %eax
902		jo _errOverflow
903		movslq %eax, %rax
904		pushq %rax
905		popq %rax
906		movq %rax, %rax
907		movq %rax, %r13
908		# Stack pointer unchanged, no stack allocated variables
909	.L12:
910		cmpq %r12, %r13
911		jl .L13
912		pushq %rdi
913		# Set up R11 as a temporary second base pointer for the caller saved things
914		movq %rsp, %r11
915		# Stack pointer unchanged, no stack allocated arguments
916		leaq .L.str0(%rip), %rax
917		pushq %rax
918		popq %rax
919		movq %rax, %rax
920		movq %rax, %rdi
921		# statement primitives do not return results (but will clobber r0/rax)
922		call _prints
923		call _println
924		popq %rdi
925		movq $1, %rax
926		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
927		movq %rbp, %rsp
928		# popq {%r12, %r13, %r14}
929		movq (%rsp), %r12
930		movq 8(%rsp), %r13
931		movq 16(%rsp), %r14
932		addq $24, %rsp
933		popq %rbp
934		ret
935		# Stack pointer unchanged, no stack allocated variables
936		# 'ere be dragons: this is 100% dead code, functions always end in returns!
937	
938	wacc_calculateIndex:
939		pushq %rbp
940		pushq %r12
941		movq %rsp, %rbp
942		# Stack pointer unchanged, no stack allocated variables
943		movslq -4(%rdi), %rax
944		movq %rax, %rax
945		movq %rax, %r12
946		movl %esi, %eax
947		cmpl $0, %r12d
948		je _errDivZero
949		# sign extend EAX into EDX
950		cltd
951		idivl %r12d
952		movl %edx, %eax
953		movl %eax, %eax
954		movslq %eax, %rax
955		pushq %rax
956		popq %rax
957		movq %rax, %rax
958		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
959		movq %rbp, %rsp
960		popq %r12
961		popq %rbp
962		ret
963		# Stack pointer unchanged, no stack allocated variables
964		# 'ere be dragons: this is 100% dead code, functions always end in returns!
965	
966	wacc_findNode:
967		pushq %rbp
968		pushq %r12
969		movq %rsp, %rbp
970		jmp .L14
971	.L15:
972		# Stack pointer unchanged, no stack allocated variables
973		cmpq $0, %rdi
974		je _errNull
975		movq (%rdi), %rax
976		movq %rax, %rax
977		movq %rax, %r12
978		cmpq %rsi, %r12
979		je .L16
980		cmpq $0, %rdi
981		je _errNull
982		movq 8(%rdi), %rax
983		movq %rax, %rax
984		pushq %rax
985		popq %rax
986		movq %rax, %rax
987		movq %rax, %rdi
988		jmp .L17
989	.L16:
990		movq %rdi, %rax
991		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
992		movq %rbp, %rsp
993		popq %r12
994		popq %rbp
995		ret
996	.L17:
997		# Stack pointer unchanged, no stack allocated variables
998	.L14:
999		cmpq $0, %rdi
1000		jne .L15
1001		movq $0, %rax
1002		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1003		movq %rbp, %rsp
1004		popq %r12
1005		popq %rbp
1006		ret
1007		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1008	
1009	wacc_removeNode:
1010		pushq %rbp
1011		pushq %r12
1012		movq %rsp, %rbp
1013		cmpq $0, %rdi
1014		je .L18
1015		cmpq %rsi, %rdi
1016		je .L20
1017		# Stack pointer unchanged, no stack allocated variables
1018		cmpq $0, %rdi
1019		je _errNull
1020		movq 8(%rdi), %rax
1021		movq %rax, %rax
1022		movq %rax, %r12
1023		# pushq {%rsi, %rdi}
1024		subq $16, %rsp
1025		movq %rsi, (%rsp)
1026		movq %rdi, 8(%rsp)
1027		# Set up R11 as a temporary second base pointer for the caller saved things
1028		movq %rsp, %r11
1029		# Stack pointer unchanged, no stack allocated arguments
1030		movq %r12, %rax
1031		movq %rax, %rdi
1032		movq %rsi, %rax
1033		movq %rax, %rsi
1034		call wacc_removeNode
1035		movq %rax, %r11
1036		# Stack pointer unchanged, no stack allocated arguments
1037		# popq {%rsi, %rdi}
1038		movq (%rsp), %rsi
1039		movq 8(%rsp), %rdi
1040		addq $16, %rsp
1041		cmpq $0, %rdi
1042		je _errNull
1043		movq %r11, %rax
1044		movq %rax, 8(%rdi)
1045		movq %rdi, %rax
1046		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1047		movq %rbp, %rsp
1048		popq %r12
1049		popq %rbp
1050		ret
1051		# Stack pointer unchanged, no stack allocated variables
1052		jmp .L21
1053	.L20:
1054		cmpq $0, %rdi
1055		je _errNull
1056		movq 8(%rdi), %rax
1057		movq %rax, %rax
1058		pushq %rax
1059		popq %rax
1060		movq %rax, %rax
1061		movq %rax, %rdi
1062		# pushq {%rsi, %rdi}
1063		subq $16, %rsp
1064		movq %rsi, (%rsp)
1065		movq %rdi, 8(%rsp)
1066		# Set up R11 as a temporary second base pointer for the caller saved things
1067		movq %rsp, %r11
1068		# Stack pointer unchanged, no stack allocated arguments
1069		movq %rsi, %rax
1070		movq %rax, %rdi
1071		# statement primitives do not return results (but will clobber r0/rax)
1072		call _freepair
1073		# popq {%rsi, %rdi}
1074		movq (%rsp), %rsi
1075		movq 8(%rsp), %rdi
1076		addq $16, %rsp
1077		movq %rdi, %rax
1078		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1079		movq %rbp, %rsp
1080		popq %r12
1081		popq %rbp
1082		ret
1083	.L21:
1084		jmp .L19
1085	.L18:
1086		movq $0, %rax
1087		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1088		movq %rbp, %rsp
1089		popq %r12
1090		popq %rbp
1091		ret
1092	.L19:
1093		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1094	
1095	wacc_countNodes:
1096		pushq %rbp
1097		pushq %r12
1098		movq %rsp, %rbp
1099		# Stack pointer unchanged, no stack allocated variables
1100		movq $0, %rax
1101		movq %rax, %r12
1102		jmp .L22
1103	.L23:
1104		movl %r12d, %eax
1105		addl $1, %eax
1106		jo _errOverflow
1107		movslq %eax, %rax
1108		pushq %rax
1109		popq %rax
1110		movq %rax, %rax
1111		movq %rax, %r12
1112		cmpq $0, %rdi
1113		je _errNull
1114		movq 8(%rdi), %rax
1115		movq %rax, %rax
1116		pushq %rax
1117		popq %rax
1118		movq %rax, %rax
1119		movq %rax, %rdi
1120	.L22:
1121		cmpq $0, %rdi
1122		jne .L23
1123		movq %r12, %rax
1124		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1125		movq %rbp, %rsp
1126		popq %r12
1127		popq %rbp
1128		ret
1129		# Stack pointer unchanged, no stack allocated variables
1130		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1131	
1132	wacc_printAllNodes:
1133		pushq %rbp
1134		pushq %r12
1135		movq %rsp, %rbp
1136		jmp .L24
1137	.L25:
1138		# Stack pointer unchanged, no stack allocated variables
1139		cmpq $0, %rdi
1140		je _errNull
1141		movq (%rdi), %rax
1142		movq %rax, %rax
1143		movq %rax, %r12
1144		pushq %rdi
1145		# Set up R11 as a temporary second base pointer for the caller saved things
1146		movq %rsp, %r11
1147		# Stack pointer unchanged, no stack allocated arguments
1148		movq %r12, %rax
1149		movq %rax, %rdi
1150		# statement primitives do not return results (but will clobber r0/rax)
1151		call _printi
1152		popq %rdi
1153		pushq %rdi
1154		# Set up R11 as a temporary second base pointer for the caller saved things
1155		movq %rsp, %r11
1156		# Stack pointer unchanged, no stack allocated arguments
1157		movq $32, %rax
1158		movq %rax, %rdi
1159		# statement primitives do not return results (but will clobber r0/rax)
1160		call _printc
1161		popq %rdi
1162		cmpq $0, %rdi
1163		je _errNull
1164		movq 8(%rdi), %rax
1165		movq %rax, %rax
1166		pushq %rax
1167		popq %rax
1168		movq %rax, %rax
1169		movq %rax, %rdi
1170		# Stack pointer unchanged, no stack allocated variables
1171	.L24:
1172		cmpq $0, %rdi
1173		jne .L25
1174		movq $1, %rax
1175		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1176		movq %rbp, %rsp
1177		popq %r12
1178		popq %rbp
1179		ret
1180		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1181	
1182	wacc_printMenu:
1183		pushq %rbp
1184		# pushq {%r12, %r13, %r14, %r15}
1185		subq $32, %rsp
1186		movq %r12, (%rsp)
1187		movq %r13, 8(%rsp)
1188		movq %r14, 16(%rsp)
1189		movq %r15, 24(%rsp)
1190		movq %rsp, %rbp
1191		# Stack pointer unchanged, no stack allocated variables
1192		# Stack pointer unchanged, no stack allocated arguments
1193		leaq .L.str1(%rip), %rax
1194		pushq %rax
1195		popq %rax
1196		movq %rax, %rax
1197		movq %rax, %rdi
1198		# statement primitives do not return results (but will clobber r0/rax)
1199		call _prints
1200		call _println
1201		# Stack pointer unchanged, no stack allocated arguments
1202		leaq .L.str2(%rip), %rax
1203		pushq %rax
1204		popq %rax
1205		movq %rax, %rax
1206		movq %rax, %rdi
1207		# statement primitives do not return results (but will clobber r0/rax)
1208		call _prints
1209		call _println
1210		# Stack pointer unchanged, no stack allocated arguments
1211		leaq .L.str1(%rip), %rax
1212		pushq %rax
1213		popq %rax
1214		movq %rax, %rax
1215		movq %rax, %rdi
1216		# statement primitives do not return results (but will clobber r0/rax)
1217		call _prints
1218		call _println
1219		# Stack pointer unchanged, no stack allocated arguments
1220		leaq .L.str3(%rip), %rax
1221		pushq %rax
1222		popq %rax
1223		movq %rax, %rax
1224		movq %rax, %rdi
1225		# statement primitives do not return results (but will clobber r0/rax)
1226		call _prints
1227		call _println
1228		# Stack pointer unchanged, no stack allocated arguments
1229		leaq .L.str4(%rip), %rax
1230		pushq %rax
1231		popq %rax
1232		movq %rax, %rax
1233		movq %rax, %rdi
1234		# statement primitives do not return results (but will clobber r0/rax)
1235		call _prints
1236		call _println
1237		# Stack pointer unchanged, no stack allocated arguments
1238		leaq .L.str3(%rip), %rax
1239		pushq %rax
1240		popq %rax
1241		movq %rax, %rax
1242		movq %rax, %rdi
1243		# statement primitives do not return results (but will clobber r0/rax)
1244		call _prints
1245		call _println
1246		# Stack pointer unchanged, no stack allocated arguments
1247		leaq .L.str5(%rip), %rax
1248		pushq %rax
1249		popq %rax
1250		movq %rax, %rax
1251		movq %rax, %rdi
1252		# statement primitives do not return results (but will clobber r0/rax)
1253		call _prints
1254		call _println
1255		# Stack pointer unchanged, no stack allocated arguments
1256		leaq .L.str6(%rip), %rax
1257		pushq %rax
1258		popq %rax
1259		movq %rax, %rax
1260		movq %rax, %rdi
1261		# statement primitives do not return results (but will clobber r0/rax)
1262		call _prints
1263		call _println
1264		# Stack pointer unchanged, no stack allocated arguments
1265		leaq .L.str7(%rip), %rax
1266		pushq %rax
1267		popq %rax
1268		movq %rax, %rax
1269		movq %rax, %rdi
1270		# statement primitives do not return results (but will clobber r0/rax)
1271		call _prints
1272		call _println
1273		# Stack pointer unchanged, no stack allocated arguments
1274		leaq .L.str8(%rip), %rax
1275		pushq %rax
1276		popq %rax
1277		movq %rax, %rax
1278		movq %rax, %rdi
1279		# statement primitives do not return results (but will clobber r0/rax)
1280		call _prints
1281		call _println
1282		# Stack pointer unchanged, no stack allocated arguments
1283		leaq .L.str9(%rip), %rax
1284		pushq %rax
1285		popq %rax
1286		movq %rax, %rax
1287		movq %rax, %rdi
1288		# statement primitives do not return results (but will clobber r0/rax)
1289		call _prints
1290		call _println
1291		# Stack pointer unchanged, no stack allocated arguments
1292		leaq .L.str10(%rip), %rax
1293		pushq %rax
1294		popq %rax
1295		movq %rax, %rax
1296		movq %rax, %rdi
1297		# statement primitives do not return results (but will clobber r0/rax)
1298		call _prints
1299		call _println
1300		# Stack pointer unchanged, no stack allocated arguments
1301		leaq .L.str11(%rip), %rax
1302		pushq %rax
1303		popq %rax
1304		movq %rax, %rax
1305		movq %rax, %rdi
1306		# statement primitives do not return results (but will clobber r0/rax)
1307		call _prints
1308		call _println
1309		# Stack pointer unchanged, no stack allocated arguments
1310		leaq .L.str3(%rip), %rax
1311		pushq %rax
1312		popq %rax
1313		movq %rax, %rax
1314		movq %rax, %rdi
1315		# statement primitives do not return results (but will clobber r0/rax)
1316		call _prints
1317		call _println
1318		# Stack pointer unchanged, no stack allocated arguments
1319		leaq .L.str1(%rip), %rax
1320		pushq %rax
1321		popq %rax
1322		movq %rax, %rax
1323		movq %rax, %rdi
1324		# statement primitives do not return results (but will clobber r0/rax)
1325		call _prints
1326		call _println
1327		movq $97, %rax
1328		movq %rax, %r12
1329		movq $103, %rax
1330		movq %rax, %r13
1331		jmp .L26
1332	.L27:
1333		# Stack pointer unchanged, no stack allocated variables
1334		# Stack pointer unchanged, no stack allocated arguments
1335		leaq .L.str12(%rip), %rax
1336		pushq %rax
1337		popq %rax
1338		movq %rax, %rax
1339		movq %rax, %rdi
1340		# statement primitives do not return results (but will clobber r0/rax)
1341		call _prints
1342		movq $0, %rax
1343		movq %rax, %r14
1344		# Stack pointer unchanged, no stack allocated arguments
1345		# load the current value in the destination of the read so it supports defaults
1346		movq %r14, %rax
1347		movq %rax, %rdi
1348		call _readc
1349		movq %rax, %r11
1350		movq %r11, %rax
1351		movq %rax, %r14
1352		movq %r14, %rax
1353		movq %rax, %r15
1354		cmpq %r15, %r12
1355		setle %al
1356		movsbq %al, %rax
1357		pushq %rax
1358		popq %rax
1359		cmpq $1, %rax
1360		jne .L30
1361		cmpq %r13, %r15
1362		setle %al
1363		movsbq %al, %rax
1364		pushq %rax
1365		popq %rax
1366		cmpq $1, %rax
1367	.L30:
1368		je .L28
1369		# Stack pointer unchanged, no stack allocated arguments
1370		leaq .L.str13(%rip), %rax
1371		pushq %rax
1372		popq %rax
1373		movq %rax, %rax
1374		movq %rax, %rdi
1375		# statement primitives do not return results (but will clobber r0/rax)
1376		call _prints
1377		# Stack pointer unchanged, no stack allocated arguments
1378		movq %r14, %rax
1379		movq %rax, %rdi
1380		# statement primitives do not return results (but will clobber r0/rax)
1381		call _printc
1382		# Stack pointer unchanged, no stack allocated arguments
1383		leaq .L.str14(%rip), %rax
1384		pushq %rax
1385		popq %rax
1386		movq %rax, %rax
1387		movq %rax, %rdi
1388		# statement primitives do not return results (but will clobber r0/rax)
1389		call _prints
1390		call _println
1391		jmp .L29
1392	.L28:
1393		movq %r14, %rax
1394		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1395		movq %rbp, %rsp
1396		# popq {%r12, %r13, %r14, %r15}
1397		movq (%rsp), %r12
1398		movq 8(%rsp), %r13
1399		movq 16(%rsp), %r14
1400		movq 24(%rsp), %r15
1401		addq $32, %rsp
1402		popq %rbp
1403		ret
1404	.L29:
1405		# Stack pointer unchanged, no stack allocated variables
1406	.L26:
1407		movq $1, %rax
1408		cmpq $1, %rax
1409		je .L27
1410		movq $0, %rax
1411		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1412		movq %rbp, %rsp
1413		# popq {%r12, %r13, %r14, %r15}
1414		movq (%rsp), %r12
1415		movq 8(%rsp), %r13
1416		movq 16(%rsp), %r14
1417		movq 24(%rsp), %r15
1418		addq $32, %rsp
1419		popq %rbp
1420		ret
1421		# Stack pointer unchanged, no stack allocated variables
1422		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1423	
1424	wacc_askForInt:
1425		pushq %rbp
1426		pushq %r12
1427		movq %rsp, %rbp
1428		# Stack pointer unchanged, no stack allocated variables
1429		pushq %rdi
1430		# Set up R11 as a temporary second base pointer for the caller saved things
1431		movq %rsp, %r11
1432		# Stack pointer unchanged, no stack allocated arguments
1433		movq %rdi, %rax
1434		movq %rax, %rdi
1435		# statement primitives do not return results (but will clobber r0/rax)
1436		call _prints
1437		popq %rdi
1438		movq $0, %rax
1439		movq %rax, %r12
1440		pushq %rdi
1441		# Set up R11 as a temporary second base pointer for the caller saved things
1442		movq %rsp, %r11
1443		# Stack pointer unchanged, no stack allocated arguments
1444		# load the current value in the destination of the read so it supports defaults
1445		movq %r12, %rax
1446		movq %rax, %rdi
1447		call _readi
1448		movq %rax, %r11
1449		popq %rdi
1450		movq %r11, %rax
1451		movq %rax, %r12
1452		pushq %rdi
1453		# Set up R11 as a temporary second base pointer for the caller saved things
1454		movq %rsp, %r11
1455		# Stack pointer unchanged, no stack allocated arguments
1456		leaq .L.str13(%rip), %rax
1457		pushq %rax
1458		popq %rax
1459		movq %rax, %rax
1460		movq %rax, %rdi
1461		# statement primitives do not return results (but will clobber r0/rax)
1462		call _prints
1463		popq %rdi
1464		pushq %rdi
1465		# Set up R11 as a temporary second base pointer for the caller saved things
1466		movq %rsp, %r11
1467		# Stack pointer unchanged, no stack allocated arguments
1468		movq %r12, %rax
1469		movq %rax, %rdi
1470		# statement primitives do not return results (but will clobber r0/rax)
1471		call _printi
1472		call _println
1473		popq %rdi
1474		movq %r12, %rax
1475		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1476		movq %rbp, %rsp
1477		popq %r12
1478		popq %rbp
1479		ret
1480		# Stack pointer unchanged, no stack allocated variables
1481		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1482	
1483	wacc_handleMenuInsert:
1484		pushq %rbp
1485		# pushq {%r12, %r13}
1486		subq $16, %rsp
1487		movq %r12, (%rsp)
1488		movq %r13, 8(%rsp)
1489		movq %rsp, %rbp
1490		# Stack pointer unchanged, no stack allocated variables
1491		pushq %rdi
1492		# Set up R11 as a temporary second base pointer for the caller saved things
1493		movq %rsp, %r11
1494		# Stack pointer unchanged, no stack allocated arguments
1495		leaq .L.str15(%rip), %rax
1496		pushq %rax
1497		popq %rax
1498		movq %rax, %rax
1499		movq %rax, %rdi
1500		call wacc_askForInt
1501		movq %rax, %r11
1502		# Stack pointer unchanged, no stack allocated arguments
1503		popq %rdi
1504		movq %r11, %rax
1505		movq %rax, %r12
1506		pushq %rdi
1507		# Set up R11 as a temporary second base pointer for the caller saved things
1508		movq %rsp, %r11
1509		# Stack pointer unchanged, no stack allocated arguments
1510		movq %rdi, %rax
1511		movq %rax, %rdi
1512		movq %r12, %rax
1513		movq %rax, %rsi
1514		call wacc_insertIfNotContain
1515		movq %rax, %r11
1516		# Stack pointer unchanged, no stack allocated arguments
1517		popq %rdi
1518		movq %r11, %rax
1519		movq %rax, %r13
1520		cmpq $1, %r13
1521		je .L31
1522		pushq %rdi
1523		# Set up R11 as a temporary second base pointer for the caller saved things
1524		movq %rsp, %r11
1525		# Stack pointer unchanged, no stack allocated arguments
1526		leaq .L.str16(%rip), %rax
1527		pushq %rax
1528		popq %rax
1529		movq %rax, %rax
1530		movq %rax, %rdi
1531		# statement primitives do not return results (but will clobber r0/rax)
1532		call _prints
1533		call _println
1534		popq %rdi
1535		jmp .L32
1536	.L31:
1537		pushq %rdi
1538		# Set up R11 as a temporary second base pointer for the caller saved things
1539		movq %rsp, %r11
1540		# Stack pointer unchanged, no stack allocated arguments
1541		leaq .L.str17(%rip), %rax
1542		pushq %rax
1543		popq %rax
1544		movq %rax, %rax
1545		movq %rax, %rdi
1546		# statement primitives do not return results (but will clobber r0/rax)
1547		call _prints
1548		call _println
1549		popq %rdi
1550	.L32:
1551		movq $1, %rax
1552		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1553		movq %rbp, %rsp
1554		# popq {%r12, %r13}
1555		movq (%rsp), %r12
1556		movq 8(%rsp), %r13
1557		addq $16, %rsp
1558		popq %rbp
1559		ret
1560		# Stack pointer unchanged, no stack allocated variables
1561		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1562	
1563	wacc_handleMenuFind:
1564		pushq %rbp
1565		# pushq {%r12, %r13}
1566		subq $16, %rsp
1567		movq %r12, (%rsp)
1568		movq %r13, 8(%rsp)
1569		movq %rsp, %rbp
1570		# Stack pointer unchanged, no stack allocated variables
1571		pushq %rdi
1572		# Set up R11 as a temporary second base pointer for the caller saved things
1573		movq %rsp, %r11
1574		# Stack pointer unchanged, no stack allocated arguments
1575		leaq .L.str18(%rip), %rax
1576		pushq %rax
1577		popq %rax
1578		movq %rax, %rax
1579		movq %rax, %rdi
1580		call wacc_askForInt
1581		movq %rax, %r11
1582		# Stack pointer unchanged, no stack allocated arguments
1583		popq %rdi
1584		movq %r11, %rax
1585		movq %rax, %r12
1586		pushq %rdi
1587		# Set up R11 as a temporary second base pointer for the caller saved things
1588		movq %rsp, %r11
1589		# Stack pointer unchanged, no stack allocated arguments
1590		movq %rdi, %rax
1591		movq %rax, %rdi
1592		movq %r12, %rax
1593		movq %rax, %rsi
1594		call wacc_contain
1595		movq %rax, %r11
1596		# Stack pointer unchanged, no stack allocated arguments
1597		popq %rdi
1598		movq %r11, %rax
1599		movq %rax, %r13
1600		cmpq $1, %r13
1601		je .L33
1602		pushq %rdi
1603		# Set up R11 as a temporary second base pointer for the caller saved things
1604		movq %rsp, %r11
1605		# Stack pointer unchanged, no stack allocated arguments
1606		leaq .L.str19(%rip), %rax
1607		pushq %rax
1608		popq %rax
1609		movq %rax, %rax
1610		movq %rax, %rdi
1611		# statement primitives do not return results (but will clobber r0/rax)
1612		call _prints
1613		call _println
1614		popq %rdi
1615		jmp .L34
1616	.L33:
1617		pushq %rdi
1618		# Set up R11 as a temporary second base pointer for the caller saved things
1619		movq %rsp, %r11
1620		# Stack pointer unchanged, no stack allocated arguments
1621		leaq .L.str20(%rip), %rax
1622		pushq %rax
1623		popq %rax
1624		movq %rax, %rax
1625		movq %rax, %rdi
1626		# statement primitives do not return results (but will clobber r0/rax)
1627		call _prints
1628		call _println
1629		popq %rdi
1630	.L34:
1631		movq $1, %rax
1632		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1633		movq %rbp, %rsp
1634		# popq {%r12, %r13}
1635		movq (%rsp), %r12
1636		movq 8(%rsp), %r13
1637		addq $16, %rsp
1638		popq %rbp
1639		ret
1640		# Stack pointer unchanged, no stack allocated variables
1641		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1642	
1643	wacc_handleMenuCount:
1644		pushq %rbp
1645		pushq %r12
1646		movq %rsp, %rbp
1647		# Stack pointer unchanged, no stack allocated variables
1648		pushq %rdi
1649		# Set up R11 as a temporary second base pointer for the caller saved things
1650		movq %rsp, %r11
1651		# Stack pointer unchanged, no stack allocated arguments
1652		movq %rdi, %rax
1653		movq %rax, %rdi
1654		call wacc_count
1655		movq %rax, %r11
1656		# Stack pointer unchanged, no stack allocated arguments
1657		popq %rdi
1658		movq %r11, %rax
1659		movq %rax, %r12
1660		cmpq $1, %r12
1661		je .L35
1662		pushq %rdi
1663		# Set up R11 as a temporary second base pointer for the caller saved things
1664		movq %rsp, %r11
1665		# Stack pointer unchanged, no stack allocated arguments
1666		leaq .L.str21(%rip), %rax
1667		pushq %rax
1668		popq %rax
1669		movq %rax, %rax
1670		movq %rax, %rdi
1671		# statement primitives do not return results (but will clobber r0/rax)
1672		call _prints
1673		popq %rdi
1674		pushq %rdi
1675		# Set up R11 as a temporary second base pointer for the caller saved things
1676		movq %rsp, %r11
1677		# Stack pointer unchanged, no stack allocated arguments
1678		movq %r12, %rax
1679		movq %rax, %rdi
1680		# statement primitives do not return results (but will clobber r0/rax)
1681		call _printi
1682		popq %rdi
1683		pushq %rdi
1684		# Set up R11 as a temporary second base pointer for the caller saved things
1685		movq %rsp, %r11
1686		# Stack pointer unchanged, no stack allocated arguments
1687		leaq .L.str22(%rip), %rax
1688		pushq %rax
1689		popq %rax
1690		movq %rax, %rax
1691		movq %rax, %rdi
1692		# statement primitives do not return results (but will clobber r0/rax)
1693		call _prints
1694		call _println
1695		popq %rdi
1696		jmp .L36
1697	.L35:
1698		pushq %rdi
1699		# Set up R11 as a temporary second base pointer for the caller saved things
1700		movq %rsp, %r11
1701		# Stack pointer unchanged, no stack allocated arguments
1702		leaq .L.str23(%rip), %rax
1703		pushq %rax
1704		popq %rax
1705		movq %rax, %rax
1706		movq %rax, %rdi
1707		# statement primitives do not return results (but will clobber r0/rax)
1708		call _prints
1709		call _println
1710		popq %rdi
1711	.L36:
1712		movq $1, %rax
1713		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1714		movq %rbp, %rsp
1715		popq %r12
1716		popq %rbp
1717		ret
1718		# Stack pointer unchanged, no stack allocated variables
1719		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1720	
1721	wacc_handleMenuPrint:
1722		pushq %rbp
1723		pushq %r12
1724		movq %rsp, %rbp
1725		# Stack pointer unchanged, no stack allocated variables
1726		pushq %rdi
1727		# Set up R11 as a temporary second base pointer for the caller saved things
1728		movq %rsp, %r11
1729		# Stack pointer unchanged, no stack allocated arguments
1730		leaq .L.str24(%rip), %rax
1731		pushq %rax
1732		popq %rax
1733		movq %rax, %rax
1734		movq %rax, %rdi
1735		# statement primitives do not return results (but will clobber r0/rax)
1736		call _prints
1737		popq %rdi
1738		pushq %rdi
1739		# Set up R11 as a temporary second base pointer for the caller saved things
1740		movq %rsp, %r11
1741		# Stack pointer unchanged, no stack allocated arguments
1742		movq %rdi, %rax
1743		movq %rax, %rdi
1744		call wacc_printAll
1745		movq %rax, %r11
1746		# Stack pointer unchanged, no stack allocated arguments
1747		popq %rdi
1748		movq %r11, %rax
1749		movq %rax, %r12
1750		movq $1, %rax
1751		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1752		movq %rbp, %rsp
1753		popq %r12
1754		popq %rbp
1755		ret
1756		# Stack pointer unchanged, no stack allocated variables
1757		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1758	
1759	wacc_handleMenuRemove:
1760		pushq %rbp
1761		# pushq {%r12, %r13}
1762		subq $16, %rsp
1763		movq %r12, (%rsp)
1764		movq %r13, 8(%rsp)
1765		movq %rsp, %rbp
1766		# Stack pointer unchanged, no stack allocated variables
1767		pushq %rdi
1768		# Set up R11 as a temporary second base pointer for the caller saved things
1769		movq %rsp, %r11
1770		# Stack pointer unchanged, no stack allocated arguments
1771		leaq .L.str25(%rip), %rax
1772		pushq %rax
1773		popq %rax
1774		movq %rax, %rax
1775		movq %rax, %rdi
1776		call wacc_askForInt
1777		movq %rax, %r11
1778		# Stack pointer unchanged, no stack allocated arguments
1779		popq %rdi
1780		movq %r11, %rax
1781		movq %rax, %r12
1782		pushq %rdi
1783		# Set up R11 as a temporary second base pointer for the caller saved things
1784		movq %rsp, %r11
1785		# Stack pointer unchanged, no stack allocated arguments
1786		movq %rdi, %rax
1787		movq %rax, %rdi
1788		movq %r12, %rax
1789		movq %rax, %rsi
1790		call wacc_remove
1791		movq %rax, %r11
1792		# Stack pointer unchanged, no stack allocated arguments
1793		popq %rdi
1794		movq %r11, %rax
1795		movq %rax, %r13
1796		cmpq $1, %r13
1797		je .L37
1798		pushq %rdi
1799		# Set up R11 as a temporary second base pointer for the caller saved things
1800		movq %rsp, %r11
1801		# Stack pointer unchanged, no stack allocated arguments
1802		leaq .L.str19(%rip), %rax
1803		pushq %rax
1804		popq %rax
1805		movq %rax, %rax
1806		movq %rax, %rdi
1807		# statement primitives do not return results (but will clobber r0/rax)
1808		call _prints
1809		call _println
1810		popq %rdi
1811		jmp .L38
1812	.L37:
1813		pushq %rdi
1814		# Set up R11 as a temporary second base pointer for the caller saved things
1815		movq %rsp, %r11
1816		# Stack pointer unchanged, no stack allocated arguments
1817		leaq .L.str26(%rip), %rax
1818		pushq %rax
1819		popq %rax
1820		movq %rax, %rax
1821		movq %rax, %rdi
1822		# statement primitives do not return results (but will clobber r0/rax)
1823		call _prints
1824		call _println
1825		popq %rdi
1826	.L38:
1827		movq $1, %rax
1828		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1829		movq %rbp, %rsp
1830		# popq {%r12, %r13}
1831		movq (%rsp), %r12
1832		movq 8(%rsp), %r13
1833		addq $16, %rsp
1834		popq %rbp
1835		ret
1836		# Stack pointer unchanged, no stack allocated variables
1837		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1838	
1839	wacc_handleMenuRemoveAll:
1840		pushq %rbp
1841		pushq %r12
1842		movq %rsp, %rbp
1843		# Stack pointer unchanged, no stack allocated variables
1844		pushq %rdi
1845		# Set up R11 as a temporary second base pointer for the caller saved things
1846		movq %rsp, %r11
1847		# Stack pointer unchanged, no stack allocated arguments
1848		movq %rdi, %rax
1849		movq %rax, %rdi
1850		call wacc_removeAll
1851		movq %rax, %r11
1852		# Stack pointer unchanged, no stack allocated arguments
1853		popq %rdi
1854		movq %r11, %rax
1855		movq %rax, %r12
1856		pushq %rdi
1857		# Set up R11 as a temporary second base pointer for the caller saved things
1858		movq %rsp, %r11
1859		# Stack pointer unchanged, no stack allocated arguments
1860		leaq .L.str27(%rip), %rax
1861		pushq %rax
1862		popq %rax
1863		movq %rax, %rax
1864		movq %rax, %rdi
1865		# statement primitives do not return results (but will clobber r0/rax)
1866		call _prints
1867		call _println
1868		popq %rdi
1869		movq $1, %rax
1870		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1871		movq %rbp, %rsp
1872		popq %r12
1873		popq %rbp
1874		ret
1875		# Stack pointer unchanged, no stack allocated variables
1876		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1877	
1878	_freepair:
1879		pushq %rbp
1880		movq %rsp, %rbp
1881		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1882		andq $-16, %rsp
1883		cmpq $0, %rdi
1884		je _errNull
1885		call free@plt
1886		movq %rbp, %rsp
1887		popq %rbp
1888		ret
1889	
1890	.section .rodata
1891	# length of .L._readc_str0
1892		.int 3
1893	.L._readc_str0:
1894		.asciz " %c"
1895	.text
1896	_readc:
1897		pushq %rbp
1898		movq %rsp, %rbp
1899		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1900		andq $-16, %rsp
1901		# RDI contains the "original" value of the destination of the read
1902		# allocate space on the stack to store the read: preserve alignment!
1903		# the passed default argument should be stored in case of EOF
1904		subq $16, %rsp
1905		movb %dil, (%rsp)
1906		leaq (%rsp), %rsi
1907		leaq .L._readc_str0(%rip), %rdi
1908		# on x86, al represents the number of SIMD registers used as variadic arguments
1909		movb $0, %al
1910		call scanf@plt
1911		movsbq (%rsp), %rax
1912		addq $16, %rsp
1913		movq %rbp, %rsp
1914		popq %rbp
1915		ret
1916	
1917	.section .rodata
1918	# length of .L._prints_str0
1919		.int 4
1920	.L._prints_str0:
1921		.asciz "%.*s"
1922	.text
1923	_prints:
1924		pushq %rbp
1925		movq %rsp, %rbp
1926		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1927		andq $-16, %rsp
1928		movq %rdi, %rdx
1929		movl -4(%rdi), %esi
1930		leaq .L._prints_str0(%rip), %rdi
1931		# on x86, al represents the number of SIMD registers used as variadic arguments
1932		movb $0, %al
1933		call printf@plt
1934		movq $0, %rdi
1935		call fflush@plt
1936		movq %rbp, %rsp
1937		popq %rbp
1938		ret
1939	
1940	.section .rodata
1941	# length of .L._readi_str0
1942		.int 2
1943	.L._readi_str0:
1944		.asciz "%d"
1945	.text
1946	_readi:
1947		pushq %rbp
1948		movq %rsp, %rbp
1949		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1950		andq $-16, %rsp
1951		# RDI contains the "original" value of the destination of the read
1952		# allocate space on the stack to store the read: preserve alignment!
1953		# the passed default argument should be stored in case of EOF
1954		subq $16, %rsp
1955		movl %edi, (%rsp)
1956		leaq (%rsp), %rsi
1957		leaq .L._readi_str0(%rip), %rdi
1958		# on x86, al represents the number of SIMD registers used as variadic arguments
1959		movb $0, %al
1960		call scanf@plt
1961		movslq (%rsp), %rax
1962		addq $16, %rsp
1963		movq %rbp, %rsp
1964		popq %rbp
1965		ret
1966	
1967	_malloc:
1968		pushq %rbp
1969		movq %rsp, %rbp
1970		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1971		andq $-16, %rsp
1972		call malloc@plt
1973		cmpq $0, %rax
1974		je _errOutOfMemory
1975		movq %rbp, %rsp
1976		popq %rbp
1977		ret
1978	
1979	.section .rodata
1980	# length of .L._printi_str0
1981		.int 2
1982	.L._printi_str0:
1983		.asciz "%d"
1984	.text
1985	_printi:
1986		pushq %rbp
1987		movq %rsp, %rbp
1988		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1989		andq $-16, %rsp
1990		movl %edi, %esi
1991		leaq .L._printi_str0(%rip), %rdi
1992		# on x86, al represents the number of SIMD registers used as variadic arguments
1993		movb $0, %al
1994		call printf@plt
1995		movq $0, %rdi
1996		call fflush@plt
1997		movq %rbp, %rsp
1998		popq %rbp
1999		ret
2000	
2001	.section .rodata
2002	# length of .L._printc_str0
2003		.int 2
2004	.L._printc_str0:
2005		.asciz "%c"
2006	.text
2007	_printc:
2008		pushq %rbp
2009		movq %rsp, %rbp
2010		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2011		andq $-16, %rsp
2012		movb %dil, %sil
2013		leaq .L._printc_str0(%rip), %rdi
2014		# on x86, al represents the number of SIMD registers used as variadic arguments
2015		movb $0, %al
2016		call printf@plt
2017		movq $0, %rdi
2018		call fflush@plt
2019		movq %rbp, %rsp
2020		popq %rbp
2021		ret
2022	
2023	.section .rodata
2024	# length of .L._println_str0
2025		.int 0
2026	.L._println_str0:
2027		.asciz ""
2028	.text
2029	_println:
2030		pushq %rbp
2031		movq %rsp, %rbp
2032		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2033		andq $-16, %rsp
2034		leaq .L._println_str0(%rip), %rdi
2035		call puts@plt
2036		movq $0, %rdi
2037		call fflush@plt
2038		movq %rbp, %rsp
2039		popq %rbp
2040		ret
2041	
2042	_exit:
2043		pushq %rbp
2044		movq %rsp, %rbp
2045		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2046		andq $-16, %rsp
2047		call exit@plt
2048		movq %rbp, %rsp
2049		popq %rbp
2050		ret
2051	
2052	_arrLoad8:
2053		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
2054		pushq %rbx
2055		cmpl $0, %r10d
2056		cmovl %r10d, %esi
2057		jl _errOutOfBounds
2058		movl -4(%r9), %ebx
2059		cmpl %ebx, %r10d
2060		cmovge %r10d, %esi
2061		jge _errOutOfBounds
2062		movq (%r9,%r10,8), %r9
2063		popq %rbx
2064		ret
2065	
2066	_arrStore8:
2067		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
2068		pushq %rbx
2069		cmpl $0, %r10d
2070		cmovl %r10d, %esi
2071		jl _errOutOfBounds
2072		movl -4(%r9), %ebx
2073		cmpl %ebx, %r10d
2074		cmovge %r10d, %esi
2075		jge _errOutOfBounds
2076		movq %rax, (%r9,%r10,8)
2077		popq %rbx
2078		ret
2079	
2080	.section .rodata
2081	# length of .L._errOutOfMemory_str0
2082		.int 27
2083	.L._errOutOfMemory_str0:
2084		.asciz "fatal error: out of memory\n"
2085	.text
2086	_errOutOfMemory:
2087		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2088		andq $-16, %rsp
2089		leaq .L._errOutOfMemory_str0(%rip), %rdi
2090		call _prints
2091		movb $-1, %dil
2092		call exit@plt
2093	
2094	.section .rodata
2095	# length of .L._errDivZero_str0
2096		.int 40
2097	.L._errDivZero_str0:
2098		.asciz "fatal error: division or modulo by zero\n"
2099	.text
2100	_errDivZero:
2101		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2102		andq $-16, %rsp
2103		leaq .L._errDivZero_str0(%rip), %rdi
2104		call _prints
2105		movb $-1, %dil
2106		call exit@plt
2107	
2108	.section .rodata
2109	# length of .L._errNull_str0
2110		.int 45
2111	.L._errNull_str0:
2112		.asciz "fatal error: null pair dereferenced or freed\n"
2113	.text
2114	_errNull:
2115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2116		andq $-16, %rsp
2117		leaq .L._errNull_str0(%rip), %rdi
2118		call _prints
2119		movb $-1, %dil
2120		call exit@plt
2121	
2122	.section .rodata
2123	# length of .L._errOutOfBounds_str0
2124		.int 42
2125	.L._errOutOfBounds_str0:
2126		.asciz "fatal error: array index %d out of bounds\n"
2127	.text
2128	_errOutOfBounds:
2129		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2130		andq $-16, %rsp
2131		leaq .L._errOutOfBounds_str0(%rip), %rdi
2132		# on x86, al represents the number of SIMD registers used as variadic arguments
2133		movb $0, %al
2134		call printf@plt
2135		movq $0, %rdi
2136		call fflush@plt
2137		movb $-1, %dil
2138		call exit@plt
2139	
2140	.section .rodata
2141	# length of .L._errOverflow_str0
2142		.int 52
2143	.L._errOverflow_str0:
2144		.asciz "fatal error: integer overflow or underflow occurred\n"
2145	.text
2146	_errOverflow:
2147		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2148		andq $-16, %rsp
2149		leaq .L._errOverflow_str0(%rip), %rdi
2150		call _prints
2151		movb $-1, %dil
2152		call exit@plt
===========================================================
-- Finished

