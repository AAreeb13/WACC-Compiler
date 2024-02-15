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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 0
5	.L.str0:
6		.asciz ""
7	# length of .L.str1
8		.int 43
9	.L.str1:
10		.asciz "==========================================="
11	# length of .L.str2
12		.int 43
13	.L.str2:
14		.asciz "========== Hash Table Program ============="
15	# length of .L.str3
16		.int 43
17	.L.str3:
18		.asciz "=                                         ="
19	# length of .L.str4
20		.int 43
21	.L.str4:
22		.asciz "= Please choose the following options:    ="
23	# length of .L.str5
24		.int 43
25	.L.str5:
26		.asciz "= a: insert an integer                    ="
27	# length of .L.str6
28		.int 43
29	.L.str6:
30		.asciz "= b: find an integer                      ="
31	# length of .L.str7
32		.int 43
33	.L.str7:
34		.asciz "= c: count the integers                   ="
35	# length of .L.str8
36		.int 43
37	.L.str8:
38		.asciz "= d: print all integers                   ="
39	# length of .L.str9
40		.int 43
41	.L.str9:
42		.asciz "= e: remove an integer                    ="
43	# length of .L.str10
44		.int 43
45	.L.str10:
46		.asciz "= f: remove all integers                  ="
47	# length of .L.str11
48		.int 43
49	.L.str11:
50		.asciz "= g: exit                                 ="
51	# length of .L.str12
52		.int 15
53	.L.str12:
54		.asciz "Your decision: "
55	# length of .L.str13
56		.int 18
57	.L.str13:
58		.asciz "You have entered: "
59	# length of .L.str14
60		.int 36
61	.L.str14:
62		.asciz " which is invalid, please try again."
63	# length of .L.str15
64		.int 35
65	.L.str15:
66		.asciz "Please enter an integer to insert: "
67	# length of .L.str16
68		.int 51
69	.L.str16:
70		.asciz "The integer is already there. No insertion is made."
71	# length of .L.str17
72		.int 43
73	.L.str17:
74		.asciz "Successfully insert it. The integer is new."
75	# length of .L.str18
76		.int 33
77	.L.str18:
78		.asciz "Please enter an integer to find: "
79	# length of .L.str19
80		.int 25
81	.L.str19:
82		.asciz "The integer is not found."
83	# length of .L.str20
84		.int 17
85	.L.str20:
86		.asciz "Find the integer."
87	# length of .L.str21
88		.int 10
89	.L.str21:
90		.asciz "There are "
91	# length of .L.str22
92		.int 10
93	.L.str22:
94		.asciz " integers."
95	# length of .L.str23
96		.int 24
97	.L.str23:
98		.asciz "There is only 1 integer."
99	# length of .L.str24
100		.int 23
101	.L.str24:
102		.asciz "Here are the integers: "
103	# length of .L.str25
104		.int 35
105	.L.str25:
106		.asciz "Please enter an integer to remove: "
107	# length of .L.str26
108		.int 29
109	.L.str26:
110		.asciz "The integer has been removed."
111	# length of .L.str27
112		.int 31
113	.L.str27:
114		.asciz "All integers have been removed."
115	# length of .L.str28
116		.int 23
117	.L.str28:
118		.asciz "Error: unknown choice ("
119	# length of .L.str29
120		.int 1
121	.L.str29:
122		.asciz ")"
123	# length of .L.str30
124		.int 13
125	.L.str30:
126		.asciz "Goodbye Human"
127	.text
128	main:
129		push rbp
130		# push {rbx, r12, r13, r14, r15}
131		sub rsp, 40
132		mov qword ptr [rsp], rbx
133		mov qword ptr [rsp + 8], r12
134		mov qword ptr [rsp + 16], r13
135		mov qword ptr [rsp + 24], r14
136		mov qword ptr [rsp + 32], r15
137		mov rbp, rsp
138		# Stack pointer unchanged, no stack allocated variables
139		# 13 element array
140		push rcx
141		mov edi, 108
142		call _malloc
143		mov r11, rax
144		pop rcx
145		# array pointers are shifted forwards by 4 bytes (to account for size)
146		mov r11, r11
147		add r11, 4
148		mov rax, 13
149		mov dword ptr [r11 - 4], eax
150		mov rax, 0
151		mov qword ptr [r11], rax
152		mov rax, 0
153		mov qword ptr [r11 + 8], rax
154		mov rax, 0
155		mov qword ptr [r11 + 16], rax
156		mov rax, 0
157		mov qword ptr [r11 + 24], rax
158		mov rax, 0
159		mov qword ptr [r11 + 32], rax
160		mov rax, 0
161		mov qword ptr [r11 + 40], rax
162		mov rax, 0
163		mov qword ptr [r11 + 48], rax
164		mov rax, 0
165		mov qword ptr [r11 + 56], rax
166		mov rax, 0
167		mov qword ptr [r11 + 64], rax
168		mov rax, 0
169		mov qword ptr [r11 + 72], rax
170		mov rax, 0
171		mov qword ptr [r11 + 80], rax
172		mov rax, 0
173		mov qword ptr [r11 + 88], rax
174		mov rax, 0
175		mov qword ptr [r11 + 96], rax
176		mov rax, r11
177		mov r12, rax
178		push rcx
179		# Set up R11 as a temporary second base pointer for the caller saved things
180		mov r11, rsp
181		# Stack pointer unchanged, no stack allocated arguments
182		mov rax, r12
183		mov rdi, rax
184		call wacc_init
185		mov r11, rax
186		# Stack pointer unchanged, no stack allocated arguments
187		pop rcx
188		mov rax, r11
189		mov r13, rax
190		mov rax, 1
191		mov r14, rax
192		jmp .L39
193	.L40:
194		# Stack pointer unchanged, no stack allocated variables
195		push rcx
196		# Set up R11 as a temporary second base pointer for the caller saved things
197		mov r11, rsp
198		# Stack pointer unchanged, no stack allocated arguments
199		call wacc_printMenu
200		mov r11, rax
201		# Stack pointer unchanged, no stack allocated arguments
202		pop rcx
203		mov rax, r11
204		mov r15, rax
205		cmp r15, 97
206		je .L41
207		cmp r15, 98
208		je .L43
209		cmp r15, 99
210		je .L45
211		cmp r15, 100
212		je .L47
213		cmp r15, 101
214		je .L49
215		cmp r15, 102
216		je .L51
217		cmp r15, 103
218		je .L53
219		push rcx
220		# Set up R11 as a temporary second base pointer for the caller saved things
221		mov r11, rsp
222		# Stack pointer unchanged, no stack allocated arguments
223		lea rax, [rip + .L.str28]
224		push rax
225		pop rax
226		mov rax, rax
227		mov rdi, rax
228		# statement primitives do not return results (but will clobber r0/rax)
229		call _prints
230		pop rcx
231		push rcx
232		# Set up R11 as a temporary second base pointer for the caller saved things
233		mov r11, rsp
234		# Stack pointer unchanged, no stack allocated arguments
235		mov rax, r15
236		mov rdi, rax
237		# statement primitives do not return results (but will clobber r0/rax)
238		call _printc
239		pop rcx
240		push rcx
241		# Set up R11 as a temporary second base pointer for the caller saved things
242		mov r11, rsp
243		# Stack pointer unchanged, no stack allocated arguments
244		lea rax, [rip + .L.str29]
245		push rax
246		pop rax
247		mov rax, rax
248		mov rdi, rax
249		# statement primitives do not return results (but will clobber r0/rax)
250		call _prints
251		call _println
252		pop rcx
253		push rcx
254		# Set up R11 as a temporary second base pointer for the caller saved things
255		mov r11, rsp
256		# Stack pointer unchanged, no stack allocated arguments
257		mov rax, -1
258		mov rdi, rax
259		# statement primitives do not return results (but will clobber r0/rax)
260		call _exit
261		pop rcx
262		jmp .L54
263	.L53:
264		push rcx
265		# Set up R11 as a temporary second base pointer for the caller saved things
266		mov r11, rsp
267		# Stack pointer unchanged, no stack allocated arguments
268		lea rax, [rip + .L.str30]
269		push rax
270		pop rax
271		mov rax, rax
272		mov rdi, rax
273		# statement primitives do not return results (but will clobber r0/rax)
274		call _prints
275		call _println
276		pop rcx
277		mov rax, 0
278		mov r14, rax
279	.L54:
280		jmp .L52
281	.L51:
282		# Stack pointer unchanged, no stack allocated variables
283		push rcx
284		# Set up R11 as a temporary second base pointer for the caller saved things
285		mov r11, rsp
286		# Stack pointer unchanged, no stack allocated arguments
287		mov rax, r12
288		mov rdi, rax
289		call wacc_handleMenuRemoveAll
290		mov r11, rax
291		# Stack pointer unchanged, no stack allocated arguments
292		pop rcx
293		mov rax, r11
294		mov rcx, rax
295		# Stack pointer unchanged, no stack allocated variables
296	.L52:
297		jmp .L50
298	.L49:
299		# Stack pointer unchanged, no stack allocated variables
300		push rcx
301		# Set up R11 as a temporary second base pointer for the caller saved things
302		mov r11, rsp
303		# Stack pointer unchanged, no stack allocated arguments
304		mov rax, r12
305		mov rdi, rax
306		call wacc_handleMenuRemove
307		mov r11, rax
308		# Stack pointer unchanged, no stack allocated arguments
309		pop rcx
310		mov rax, r11
311		mov rcx, rax
312		# Stack pointer unchanged, no stack allocated variables
313	.L50:
314		jmp .L48
315	.L47:
316		# Stack pointer unchanged, no stack allocated variables
317		push rcx
318		# Set up R11 as a temporary second base pointer for the caller saved things
319		mov r11, rsp
320		# Stack pointer unchanged, no stack allocated arguments
321		mov rax, r12
322		mov rdi, rax
323		call wacc_handleMenuPrint
324		mov r11, rax
325		# Stack pointer unchanged, no stack allocated arguments
326		pop rcx
327		mov rax, r11
328		mov rcx, rax
329		# Stack pointer unchanged, no stack allocated variables
330	.L48:
331		jmp .L46
332	.L45:
333		# Stack pointer unchanged, no stack allocated variables
334		push rcx
335		# Set up R11 as a temporary second base pointer for the caller saved things
336		mov r11, rsp
337		# Stack pointer unchanged, no stack allocated arguments
338		mov rax, r12
339		mov rdi, rax
340		call wacc_handleMenuCount
341		mov r11, rax
342		# Stack pointer unchanged, no stack allocated arguments
343		pop rcx
344		mov rax, r11
345		mov rcx, rax
346		# Stack pointer unchanged, no stack allocated variables
347	.L46:
348		jmp .L44
349	.L43:
350		# Stack pointer unchanged, no stack allocated variables
351		push rcx
352		# Set up R11 as a temporary second base pointer for the caller saved things
353		mov r11, rsp
354		# Stack pointer unchanged, no stack allocated arguments
355		mov rax, r12
356		mov rdi, rax
357		call wacc_handleMenuFind
358		mov r11, rax
359		# Stack pointer unchanged, no stack allocated arguments
360		pop rcx
361		mov rax, r11
362		mov rcx, rax
363		# Stack pointer unchanged, no stack allocated variables
364	.L44:
365		jmp .L42
366	.L41:
367		# Stack pointer unchanged, no stack allocated variables
368		push rcx
369		# Set up R11 as a temporary second base pointer for the caller saved things
370		mov r11, rsp
371		# Stack pointer unchanged, no stack allocated arguments
372		mov rax, r12
373		mov rdi, rax
374		call wacc_handleMenuInsert
375		mov r11, rax
376		# Stack pointer unchanged, no stack allocated arguments
377		pop rcx
378		mov rax, r11
379		mov rcx, rax
380		# Stack pointer unchanged, no stack allocated variables
381	.L42:
382		# Stack pointer unchanged, no stack allocated variables
383	.L39:
384		cmp r14, 1
385		je .L40
386		# Stack pointer unchanged, no stack allocated variables
387		mov rax, 0
388		# pop {rbx, r12, r13, r14, r15}
389		mov rbx, qword ptr [rsp]
390		mov r12, qword ptr [rsp + 8]
391		mov r13, qword ptr [rsp + 16]
392		mov r14, qword ptr [rsp + 24]
393		mov r15, qword ptr [rsp + 32]
394		add rsp, 40
395		pop rbp
396		ret
397	
398	wacc_init:
399		push rbp
400		# push {r12, r13}
401		sub rsp, 16
402		mov qword ptr [rsp], r12
403		mov qword ptr [rsp + 8], r13
404		mov rbp, rsp
405		# Stack pointer unchanged, no stack allocated variables
406		movsx rax, dword ptr [rdi - 4]
407		mov rax, rax
408		mov r12, rax
409		mov rax, 0
410		mov r13, rax
411		jmp .L0
412	.L1:
413		mov r10d, r13d
414		mov rax, 0
415		mov r9, rdi
416		call _arrStore8
417		mov eax, r13d
418		add eax, 1
419		jo _errOverflow
420		movsx rax, eax
421		push rax
422		pop rax
423		mov rax, rax
424		mov r13, rax
425	.L0:
426		cmp r13, r12
427		jl .L1
428		mov rax, 1
429		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
430		mov rsp, rbp
431		# pop {r12, r13}
432		mov r12, qword ptr [rsp]
433		mov r13, qword ptr [rsp + 8]
434		add rsp, 16
435		pop rbp
436		ret
437		# Stack pointer unchanged, no stack allocated variables
438		# 'ere be dragons: this is 100% dead code, functions always end in returns!
439	
440	wacc_contain:
441		push rbp
442		# push {r12, r13}
443		sub rsp, 16
444		mov qword ptr [rsp], r12
445		mov qword ptr [rsp + 8], r13
446		mov rbp, rsp
447		# Stack pointer unchanged, no stack allocated variables
448		# push {rsi, rdi}
449		sub rsp, 16
450		mov qword ptr [rsp], rsi
451		mov qword ptr [rsp + 8], rdi
452		# Set up R11 as a temporary second base pointer for the caller saved things
453		mov r11, rsp
454		# Stack pointer unchanged, no stack allocated arguments
455		mov rax, rdi
456		mov rdi, rax
457		mov rax, rsi
458		mov rsi, rax
459		call wacc_calculateIndex
460		mov r11, rax
461		# Stack pointer unchanged, no stack allocated arguments
462		# pop {rsi, rdi}
463		mov rsi, qword ptr [rsp]
464		mov rdi, qword ptr [rsp + 8]
465		add rsp, 16
466		mov rax, r11
467		mov r12, rax
468		# push {rsi, rdi}
469		sub rsp, 16
470		mov qword ptr [rsp], rsi
471		mov qword ptr [rsp + 8], rdi
472		# Set up R11 as a temporary second base pointer for the caller saved things
473		mov r11, rsp
474		# Stack pointer unchanged, no stack allocated arguments
475		mov r10d, r12d
476		mov r9, rdi
477		call _arrLoad8
478		mov rax, r9
479		mov rax, rax
480		mov rdi, rax
481		mov rax, rsi
482		mov rsi, rax
483		call wacc_findNode
484		mov r11, rax
485		# Stack pointer unchanged, no stack allocated arguments
486		# pop {rsi, rdi}
487		mov rsi, qword ptr [rsp]
488		mov rdi, qword ptr [rsp + 8]
489		add rsp, 16
490		mov rax, r11
491		mov r13, rax
492		cmp r13, 0
493		setne al
494		movsx rax, al
495		push rax
496		pop rax
497		mov rax, rax
498		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
499		mov rsp, rbp
500		# pop {r12, r13}
501		mov r12, qword ptr [rsp]
502		mov r13, qword ptr [rsp + 8]
503		add rsp, 16
504		pop rbp
505		ret
506		# Stack pointer unchanged, no stack allocated variables
507		# 'ere be dragons: this is 100% dead code, functions always end in returns!
508	
509	wacc_insertIfNotContain:
510		push rbp
511		# push {r12, r13, r14}
512		sub rsp, 24
513		mov qword ptr [rsp], r12
514		mov qword ptr [rsp + 8], r13
515		mov qword ptr [rsp + 16], r14
516		mov rbp, rsp
517		# Stack pointer unchanged, no stack allocated variables
518		# push {rsi, rdi}
519		sub rsp, 16
520		mov qword ptr [rsp], rsi
521		mov qword ptr [rsp + 8], rdi
522		# Set up R11 as a temporary second base pointer for the caller saved things
523		mov r11, rsp
524		# Stack pointer unchanged, no stack allocated arguments
525		mov rax, rdi
526		mov rdi, rax
527		mov rax, rsi
528		mov rsi, rax
529		call wacc_calculateIndex
530		mov r11, rax
531		# Stack pointer unchanged, no stack allocated arguments
532		# pop {rsi, rdi}
533		mov rsi, qword ptr [rsp]
534		mov rdi, qword ptr [rsp + 8]
535		add rsp, 16
536		mov rax, r11
537		mov r12, rax
538		# push {rsi, rdi}
539		sub rsp, 16
540		mov qword ptr [rsp], rsi
541		mov qword ptr [rsp + 8], rdi
542		# Set up R11 as a temporary second base pointer for the caller saved things
543		mov r11, rsp
544		# Stack pointer unchanged, no stack allocated arguments
545		mov r10d, r12d
546		mov r9, rdi
547		call _arrLoad8
548		mov rax, r9
549		mov rax, rax
550		mov rdi, rax
551		mov rax, rsi
552		mov rsi, rax
553		call wacc_findNode
554		mov r11, rax
555		# Stack pointer unchanged, no stack allocated arguments
556		# pop {rsi, rdi}
557		mov rsi, qword ptr [rsp]
558		mov rdi, qword ptr [rsp + 8]
559		add rsp, 16
560		mov rax, r11
561		mov r13, rax
562		cmp r13, 0
563		jne .L2
564		# Stack pointer unchanged, no stack allocated variables
565		# push {rsi, rdi}
566		sub rsp, 16
567		mov qword ptr [rsp], rsi
568		mov qword ptr [rsp + 8], rdi
569		mov edi, 16
570		call _malloc
571		mov r11, rax
572		# pop {rsi, rdi}
573		mov rsi, qword ptr [rsp]
574		mov rdi, qword ptr [rsp + 8]
575		add rsp, 16
576		mov rax, rsi
577		mov qword ptr [r11], rax
578		mov r10d, r12d
579		mov r9, rdi
580		call _arrLoad8
581		mov rax, r9
582		mov rax, rax
583		mov qword ptr [r11 + 8], rax
584		mov rax, r11
585		mov r14, rax
586		mov r10d, r12d
587		mov rax, r14
588		mov r9, rdi
589		call _arrStore8
590		mov rax, 1
591		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
592		mov rsp, rbp
593		# pop {r12, r13, r14}
594		mov r12, qword ptr [rsp]
595		mov r13, qword ptr [rsp + 8]
596		mov r14, qword ptr [rsp + 16]
597		add rsp, 24
598		pop rbp
599		ret
600		# Stack pointer unchanged, no stack allocated variables
601		jmp .L3
602	.L2:
603		mov rax, 0
604		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
605		mov rsp, rbp
606		# pop {r12, r13, r14}
607		mov r12, qword ptr [rsp]
608		mov r13, qword ptr [rsp + 8]
609		mov r14, qword ptr [rsp + 16]
610		add rsp, 24
611		pop rbp
612		ret
613	.L3:
614		# Stack pointer unchanged, no stack allocated variables
615		# 'ere be dragons: this is 100% dead code, functions always end in returns!
616	
617	wacc_remove:
618		push rbp
619		# push {r12, r13}
620		sub rsp, 16
621		mov qword ptr [rsp], r12
622		mov qword ptr [rsp + 8], r13
623		mov rbp, rsp
624		# Stack pointer unchanged, no stack allocated variables
625		# push {rsi, rdi}
626		sub rsp, 16
627		mov qword ptr [rsp], rsi
628		mov qword ptr [rsp + 8], rdi
629		# Set up R11 as a temporary second base pointer for the caller saved things
630		mov r11, rsp
631		# Stack pointer unchanged, no stack allocated arguments
632		mov rax, rdi
633		mov rdi, rax
634		mov rax, rsi
635		mov rsi, rax
636		call wacc_calculateIndex
637		mov r11, rax
638		# Stack pointer unchanged, no stack allocated arguments
639		# pop {rsi, rdi}
640		mov rsi, qword ptr [rsp]
641		mov rdi, qword ptr [rsp + 8]
642		add rsp, 16
643		mov rax, r11
644		mov r12, rax
645		# push {rsi, rdi}
646		sub rsp, 16
647		mov qword ptr [rsp], rsi
648		mov qword ptr [rsp + 8], rdi
649		# Set up R11 as a temporary second base pointer for the caller saved things
650		mov r11, rsp
651		# Stack pointer unchanged, no stack allocated arguments
652		mov r10d, r12d
653		mov r9, rdi
654		call _arrLoad8
655		mov rax, r9
656		mov rax, rax
657		mov rdi, rax
658		mov rax, rsi
659		mov rsi, rax
660		call wacc_findNode
661		mov r11, rax
662		# Stack pointer unchanged, no stack allocated arguments
663		# pop {rsi, rdi}
664		mov rsi, qword ptr [rsp]
665		mov rdi, qword ptr [rsp + 8]
666		add rsp, 16
667		mov rax, r11
668		mov r13, rax
669		cmp r13, 0
670		je .L4
671		# push {rsi, rdi}
672		sub rsp, 16
673		mov qword ptr [rsp], rsi
674		mov qword ptr [rsp + 8], rdi
675		# Set up R11 as a temporary second base pointer for the caller saved things
676		mov r11, rsp
677		# Stack pointer unchanged, no stack allocated arguments
678		mov r10d, r12d
679		mov r9, rdi
680		call _arrLoad8
681		mov rax, r9
682		mov rax, rax
683		mov rdi, rax
684		mov rax, r13
685		mov rsi, rax
686		call wacc_removeNode
687		mov r11, rax
688		# Stack pointer unchanged, no stack allocated arguments
689		# pop {rsi, rdi}
690		mov rsi, qword ptr [rsp]
691		mov rdi, qword ptr [rsp + 8]
692		add rsp, 16
693		mov r10d, r12d
694		mov rax, r11
695		mov r9, rdi
696		call _arrStore8
697		mov rax, 1
698		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
699		mov rsp, rbp
700		# pop {r12, r13}
701		mov r12, qword ptr [rsp]
702		mov r13, qword ptr [rsp + 8]
703		add rsp, 16
704		pop rbp
705		ret
706		jmp .L5
707	.L4:
708		mov rax, 0
709		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
710		mov rsp, rbp
711		# pop {r12, r13}
712		mov r12, qword ptr [rsp]
713		mov r13, qword ptr [rsp + 8]
714		add rsp, 16
715		pop rbp
716		ret
717	.L5:
718		# Stack pointer unchanged, no stack allocated variables
719		# 'ere be dragons: this is 100% dead code, functions always end in returns!
720	
721	wacc_removeAll:
722		push rbp
723		# push {r12, r13, r14, r15}
724		sub rsp, 32
725		mov qword ptr [rsp], r12
726		mov qword ptr [rsp + 8], r13
727		mov qword ptr [rsp + 16], r14
728		mov qword ptr [rsp + 24], r15
729		mov rbp, rsp
730		# Stack pointer unchanged, no stack allocated variables
731		movsx rax, dword ptr [rdi - 4]
732		mov rax, rax
733		mov r12, rax
734		mov rax, 0
735		mov r13, rax
736		jmp .L6
737	.L7:
738		# Stack pointer unchanged, no stack allocated variables
739		mov r10d, r13d
740		mov r9, rdi
741		call _arrLoad8
742		mov rax, r9
743		mov rax, rax
744		mov r14, rax
745		jmp .L8
746	.L9:
747		# Stack pointer unchanged, no stack allocated variables
748		cmp r14, 0
749		je _errNull
750		mov rax, qword ptr [r14 + 8]
751		mov rax, rax
752		mov r15, rax
753		push rdi
754		# Set up R11 as a temporary second base pointer for the caller saved things
755		mov r11, rsp
756		# Stack pointer unchanged, no stack allocated arguments
757		mov rax, r14
758		mov rdi, rax
759		# statement primitives do not return results (but will clobber r0/rax)
760		call _freepair
761		pop rdi
762		mov rax, r15
763		mov r14, rax
764		# Stack pointer unchanged, no stack allocated variables
765	.L8:
766		cmp r14, 0
767		jne .L9
768		mov r10d, r13d
769		mov rax, 0
770		mov r9, rdi
771		call _arrStore8
772		mov eax, r13d
773		add eax, 1
774		jo _errOverflow
775		movsx rax, eax
776		push rax
777		pop rax
778		mov rax, rax
779		mov r13, rax
780		# Stack pointer unchanged, no stack allocated variables
781	.L6:
782		cmp r13, r12
783		jl .L7
784		mov rax, 1
785		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
786		mov rsp, rbp
787		# pop {r12, r13, r14, r15}
788		mov r12, qword ptr [rsp]
789		mov r13, qword ptr [rsp + 8]
790		mov r14, qword ptr [rsp + 16]
791		mov r15, qword ptr [rsp + 24]
792		add rsp, 32
793		pop rbp
794		ret
795		# Stack pointer unchanged, no stack allocated variables
796		# 'ere be dragons: this is 100% dead code, functions always end in returns!
797	
798	wacc_count:
799		push rbp
800		# push {r12, r13, r14, r15}
801		sub rsp, 32
802		mov qword ptr [rsp], r12
803		mov qword ptr [rsp + 8], r13
804		mov qword ptr [rsp + 16], r14
805		mov qword ptr [rsp + 24], r15
806		mov rbp, rsp
807		# Stack pointer unchanged, no stack allocated variables
808		movsx rax, dword ptr [rdi - 4]
809		mov rax, rax
810		mov r12, rax
811		mov rax, 0
812		mov r13, rax
813		mov rax, 0
814		mov r14, rax
815		jmp .L10
816	.L11:
817		# Stack pointer unchanged, no stack allocated variables
818		push rdi
819		# Set up R11 as a temporary second base pointer for the caller saved things
820		mov r11, rsp
821		# Stack pointer unchanged, no stack allocated arguments
822		mov r10d, r14d
823		mov r9, rdi
824		call _arrLoad8
825		mov rax, r9
826		mov rax, rax
827		mov rdi, rax
828		call wacc_countNodes
829		mov r11, rax
830		# Stack pointer unchanged, no stack allocated arguments
831		pop rdi
832		mov rax, r11
833		mov r15, rax
834		mov eax, r13d
835		add eax, r15d
836		jo _errOverflow
837		movsx rax, eax
838		push rax
839		pop rax
840		mov rax, rax
841		mov r13, rax
842		mov eax, r14d
843		add eax, 1
844		jo _errOverflow
845		movsx rax, eax
846		push rax
847		pop rax
848		mov rax, rax
849		mov r14, rax
850		# Stack pointer unchanged, no stack allocated variables
851	.L10:
852		cmp r14, r12
853		jl .L11
854		mov rax, r13
855		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
856		mov rsp, rbp
857		# pop {r12, r13, r14, r15}
858		mov r12, qword ptr [rsp]
859		mov r13, qword ptr [rsp + 8]
860		mov r14, qword ptr [rsp + 16]
861		mov r15, qword ptr [rsp + 24]
862		add rsp, 32
863		pop rbp
864		ret
865		# Stack pointer unchanged, no stack allocated variables
866		# 'ere be dragons: this is 100% dead code, functions always end in returns!
867	
868	wacc_printAll:
869		push rbp
870		# push {r12, r13, r14}
871		sub rsp, 24
872		mov qword ptr [rsp], r12
873		mov qword ptr [rsp + 8], r13
874		mov qword ptr [rsp + 16], r14
875		mov rbp, rsp
876		# Stack pointer unchanged, no stack allocated variables
877		movsx rax, dword ptr [rdi - 4]
878		mov rax, rax
879		mov r12, rax
880		mov rax, 0
881		mov r13, rax
882		jmp .L12
883	.L13:
884		# Stack pointer unchanged, no stack allocated variables
885		push rdi
886		# Set up R11 as a temporary second base pointer for the caller saved things
887		mov r11, rsp
888		# Stack pointer unchanged, no stack allocated arguments
889		mov r10d, r13d
890		mov r9, rdi
891		call _arrLoad8
892		mov rax, r9
893		mov rax, rax
894		mov rdi, rax
895		call wacc_printAllNodes
896		mov r11, rax
897		# Stack pointer unchanged, no stack allocated arguments
898		pop rdi
899		mov rax, r11
900		mov r14, rax
901		mov eax, r13d
902		add eax, 1
903		jo _errOverflow
904		movsx rax, eax
905		push rax
906		pop rax
907		mov rax, rax
908		mov r13, rax
909		# Stack pointer unchanged, no stack allocated variables
910	.L12:
911		cmp r13, r12
912		jl .L13
913		push rdi
914		# Set up R11 as a temporary second base pointer for the caller saved things
915		mov r11, rsp
916		# Stack pointer unchanged, no stack allocated arguments
917		lea rax, [rip + .L.str0]
918		push rax
919		pop rax
920		mov rax, rax
921		mov rdi, rax
922		# statement primitives do not return results (but will clobber r0/rax)
923		call _prints
924		call _println
925		pop rdi
926		mov rax, 1
927		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
928		mov rsp, rbp
929		# pop {r12, r13, r14}
930		mov r12, qword ptr [rsp]
931		mov r13, qword ptr [rsp + 8]
932		mov r14, qword ptr [rsp + 16]
933		add rsp, 24
934		pop rbp
935		ret
936		# Stack pointer unchanged, no stack allocated variables
937		# 'ere be dragons: this is 100% dead code, functions always end in returns!
938	
939	wacc_calculateIndex:
940		push rbp
941		push r12
942		mov rbp, rsp
943		# Stack pointer unchanged, no stack allocated variables
944		movsx rax, dword ptr [rdi - 4]
945		mov rax, rax
946		mov r12, rax
947		mov eax, esi
948		cmp r12d, 0
949		je _errDivZero
950		# sign extend EAX into EDX
951		cdq
952		idiv r12d
953		mov eax, edx
954		mov eax, eax
955		movsx rax, eax
956		push rax
957		pop rax
958		mov rax, rax
959		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
960		mov rsp, rbp
961		pop r12
962		pop rbp
963		ret
964		# Stack pointer unchanged, no stack allocated variables
965		# 'ere be dragons: this is 100% dead code, functions always end in returns!
966	
967	wacc_findNode:
968		push rbp
969		push r12
970		mov rbp, rsp
971		jmp .L14
972	.L15:
973		# Stack pointer unchanged, no stack allocated variables
974		cmp rdi, 0
975		je _errNull
976		mov rax, qword ptr [rdi]
977		mov rax, rax
978		mov r12, rax
979		cmp r12, rsi
980		je .L16
981		cmp rdi, 0
982		je _errNull
983		mov rax, qword ptr [rdi + 8]
984		mov rax, rax
985		push rax
986		pop rax
987		mov rax, rax
988		mov rdi, rax
989		jmp .L17
990	.L16:
991		mov rax, rdi
992		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
993		mov rsp, rbp
994		pop r12
995		pop rbp
996		ret
997	.L17:
998		# Stack pointer unchanged, no stack allocated variables
999	.L14:
1000		cmp rdi, 0
1001		jne .L15
1002		mov rax, 0
1003		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1004		mov rsp, rbp
1005		pop r12
1006		pop rbp
1007		ret
1008		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1009	
1010	wacc_removeNode:
1011		push rbp
1012		push r12
1013		mov rbp, rsp
1014		cmp rdi, 0
1015		je .L18
1016		cmp rdi, rsi
1017		je .L20
1018		# Stack pointer unchanged, no stack allocated variables
1019		cmp rdi, 0
1020		je _errNull
1021		mov rax, qword ptr [rdi + 8]
1022		mov rax, rax
1023		mov r12, rax
1024		# push {rsi, rdi}
1025		sub rsp, 16
1026		mov qword ptr [rsp], rsi
1027		mov qword ptr [rsp + 8], rdi
1028		# Set up R11 as a temporary second base pointer for the caller saved things
1029		mov r11, rsp
1030		# Stack pointer unchanged, no stack allocated arguments
1031		mov rax, r12
1032		mov rdi, rax
1033		mov rax, rsi
1034		mov rsi, rax
1035		call wacc_removeNode
1036		mov r11, rax
1037		# Stack pointer unchanged, no stack allocated arguments
1038		# pop {rsi, rdi}
1039		mov rsi, qword ptr [rsp]
1040		mov rdi, qword ptr [rsp + 8]
1041		add rsp, 16
1042		cmp rdi, 0
1043		je _errNull
1044		mov rax, r11
1045		mov qword ptr [rdi + 8], rax
1046		mov rax, rdi
1047		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1048		mov rsp, rbp
1049		pop r12
1050		pop rbp
1051		ret
1052		# Stack pointer unchanged, no stack allocated variables
1053		jmp .L21
1054	.L20:
1055		cmp rdi, 0
1056		je _errNull
1057		mov rax, qword ptr [rdi + 8]
1058		mov rax, rax
1059		push rax
1060		pop rax
1061		mov rax, rax
1062		mov rdi, rax
1063		# push {rsi, rdi}
1064		sub rsp, 16
1065		mov qword ptr [rsp], rsi
1066		mov qword ptr [rsp + 8], rdi
1067		# Set up R11 as a temporary second base pointer for the caller saved things
1068		mov r11, rsp
1069		# Stack pointer unchanged, no stack allocated arguments
1070		mov rax, rsi
1071		mov rdi, rax
1072		# statement primitives do not return results (but will clobber r0/rax)
1073		call _freepair
1074		# pop {rsi, rdi}
1075		mov rsi, qword ptr [rsp]
1076		mov rdi, qword ptr [rsp + 8]
1077		add rsp, 16
1078		mov rax, rdi
1079		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1080		mov rsp, rbp
1081		pop r12
1082		pop rbp
1083		ret
1084	.L21:
1085		jmp .L19
1086	.L18:
1087		mov rax, 0
1088		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1089		mov rsp, rbp
1090		pop r12
1091		pop rbp
1092		ret
1093	.L19:
1094		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1095	
1096	wacc_countNodes:
1097		push rbp
1098		push r12
1099		mov rbp, rsp
1100		# Stack pointer unchanged, no stack allocated variables
1101		mov rax, 0
1102		mov r12, rax
1103		jmp .L22
1104	.L23:
1105		mov eax, r12d
1106		add eax, 1
1107		jo _errOverflow
1108		movsx rax, eax
1109		push rax
1110		pop rax
1111		mov rax, rax
1112		mov r12, rax
1113		cmp rdi, 0
1114		je _errNull
1115		mov rax, qword ptr [rdi + 8]
1116		mov rax, rax
1117		push rax
1118		pop rax
1119		mov rax, rax
1120		mov rdi, rax
1121	.L22:
1122		cmp rdi, 0
1123		jne .L23
1124		mov rax, r12
1125		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1126		mov rsp, rbp
1127		pop r12
1128		pop rbp
1129		ret
1130		# Stack pointer unchanged, no stack allocated variables
1131		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1132	
1133	wacc_printAllNodes:
1134		push rbp
1135		push r12
1136		mov rbp, rsp
1137		jmp .L24
1138	.L25:
1139		# Stack pointer unchanged, no stack allocated variables
1140		cmp rdi, 0
1141		je _errNull
1142		mov rax, qword ptr [rdi]
1143		mov rax, rax
1144		mov r12, rax
1145		push rdi
1146		# Set up R11 as a temporary second base pointer for the caller saved things
1147		mov r11, rsp
1148		# Stack pointer unchanged, no stack allocated arguments
1149		mov rax, r12
1150		mov rdi, rax
1151		# statement primitives do not return results (but will clobber r0/rax)
1152		call _printi
1153		pop rdi
1154		push rdi
1155		# Set up R11 as a temporary second base pointer for the caller saved things
1156		mov r11, rsp
1157		# Stack pointer unchanged, no stack allocated arguments
1158		mov rax, 32
1159		mov rdi, rax
1160		# statement primitives do not return results (but will clobber r0/rax)
1161		call _printc
1162		pop rdi
1163		cmp rdi, 0
1164		je _errNull
1165		mov rax, qword ptr [rdi + 8]
1166		mov rax, rax
1167		push rax
1168		pop rax
1169		mov rax, rax
1170		mov rdi, rax
1171		# Stack pointer unchanged, no stack allocated variables
1172	.L24:
1173		cmp rdi, 0
1174		jne .L25
1175		mov rax, 1
1176		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1177		mov rsp, rbp
1178		pop r12
1179		pop rbp
1180		ret
1181		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1182	
1183	wacc_printMenu:
1184		push rbp
1185		# push {r12, r13, r14, r15}
1186		sub rsp, 32
1187		mov qword ptr [rsp], r12
1188		mov qword ptr [rsp + 8], r13
1189		mov qword ptr [rsp + 16], r14
1190		mov qword ptr [rsp + 24], r15
1191		mov rbp, rsp
1192		# Stack pointer unchanged, no stack allocated variables
1193		# Stack pointer unchanged, no stack allocated arguments
1194		lea rax, [rip + .L.str1]
1195		push rax
1196		pop rax
1197		mov rax, rax
1198		mov rdi, rax
1199		# statement primitives do not return results (but will clobber r0/rax)
1200		call _prints
1201		call _println
1202		# Stack pointer unchanged, no stack allocated arguments
1203		lea rax, [rip + .L.str2]
1204		push rax
1205		pop rax
1206		mov rax, rax
1207		mov rdi, rax
1208		# statement primitives do not return results (but will clobber r0/rax)
1209		call _prints
1210		call _println
1211		# Stack pointer unchanged, no stack allocated arguments
1212		lea rax, [rip + .L.str1]
1213		push rax
1214		pop rax
1215		mov rax, rax
1216		mov rdi, rax
1217		# statement primitives do not return results (but will clobber r0/rax)
1218		call _prints
1219		call _println
1220		# Stack pointer unchanged, no stack allocated arguments
1221		lea rax, [rip + .L.str3]
1222		push rax
1223		pop rax
1224		mov rax, rax
1225		mov rdi, rax
1226		# statement primitives do not return results (but will clobber r0/rax)
1227		call _prints
1228		call _println
1229		# Stack pointer unchanged, no stack allocated arguments
1230		lea rax, [rip + .L.str4]
1231		push rax
1232		pop rax
1233		mov rax, rax
1234		mov rdi, rax
1235		# statement primitives do not return results (but will clobber r0/rax)
1236		call _prints
1237		call _println
1238		# Stack pointer unchanged, no stack allocated arguments
1239		lea rax, [rip + .L.str3]
1240		push rax
1241		pop rax
1242		mov rax, rax
1243		mov rdi, rax
1244		# statement primitives do not return results (but will clobber r0/rax)
1245		call _prints
1246		call _println
1247		# Stack pointer unchanged, no stack allocated arguments
1248		lea rax, [rip + .L.str5]
1249		push rax
1250		pop rax
1251		mov rax, rax
1252		mov rdi, rax
1253		# statement primitives do not return results (but will clobber r0/rax)
1254		call _prints
1255		call _println
1256		# Stack pointer unchanged, no stack allocated arguments
1257		lea rax, [rip + .L.str6]
1258		push rax
1259		pop rax
1260		mov rax, rax
1261		mov rdi, rax
1262		# statement primitives do not return results (but will clobber r0/rax)
1263		call _prints
1264		call _println
1265		# Stack pointer unchanged, no stack allocated arguments
1266		lea rax, [rip + .L.str7]
1267		push rax
1268		pop rax
1269		mov rax, rax
1270		mov rdi, rax
1271		# statement primitives do not return results (but will clobber r0/rax)
1272		call _prints
1273		call _println
1274		# Stack pointer unchanged, no stack allocated arguments
1275		lea rax, [rip + .L.str8]
1276		push rax
1277		pop rax
1278		mov rax, rax
1279		mov rdi, rax
1280		# statement primitives do not return results (but will clobber r0/rax)
1281		call _prints
1282		call _println
1283		# Stack pointer unchanged, no stack allocated arguments
1284		lea rax, [rip + .L.str9]
1285		push rax
1286		pop rax
1287		mov rax, rax
1288		mov rdi, rax
1289		# statement primitives do not return results (but will clobber r0/rax)
1290		call _prints
1291		call _println
1292		# Stack pointer unchanged, no stack allocated arguments
1293		lea rax, [rip + .L.str10]
1294		push rax
1295		pop rax
1296		mov rax, rax
1297		mov rdi, rax
1298		# statement primitives do not return results (but will clobber r0/rax)
1299		call _prints
1300		call _println
1301		# Stack pointer unchanged, no stack allocated arguments
1302		lea rax, [rip + .L.str11]
1303		push rax
1304		pop rax
1305		mov rax, rax
1306		mov rdi, rax
1307		# statement primitives do not return results (but will clobber r0/rax)
1308		call _prints
1309		call _println
1310		# Stack pointer unchanged, no stack allocated arguments
1311		lea rax, [rip + .L.str3]
1312		push rax
1313		pop rax
1314		mov rax, rax
1315		mov rdi, rax
1316		# statement primitives do not return results (but will clobber r0/rax)
1317		call _prints
1318		call _println
1319		# Stack pointer unchanged, no stack allocated arguments
1320		lea rax, [rip + .L.str1]
1321		push rax
1322		pop rax
1323		mov rax, rax
1324		mov rdi, rax
1325		# statement primitives do not return results (but will clobber r0/rax)
1326		call _prints
1327		call _println
1328		mov rax, 97
1329		mov r12, rax
1330		mov rax, 103
1331		mov r13, rax
1332		jmp .L26
1333	.L27:
1334		# Stack pointer unchanged, no stack allocated variables
1335		# Stack pointer unchanged, no stack allocated arguments
1336		lea rax, [rip + .L.str12]
1337		push rax
1338		pop rax
1339		mov rax, rax
1340		mov rdi, rax
1341		# statement primitives do not return results (but will clobber r0/rax)
1342		call _prints
1343		mov rax, 0
1344		mov r14, rax
1345		# Stack pointer unchanged, no stack allocated arguments
1346		# load the current value in the destination of the read so it supports defaults
1347		mov rax, r14
1348		mov rdi, rax
1349		call _readc
1350		mov r11, rax
1351		mov rax, r11
1352		mov r14, rax
1353		mov rax, r14
1354		mov r15, rax
1355		cmp r12, r15
1356		setle al
1357		movsx rax, al
1358		push rax
1359		pop rax
1360		cmp rax, 1
1361		jne .L30
1362		cmp r15, r13
1363		setle al
1364		movsx rax, al
1365		push rax
1366		pop rax
1367		cmp rax, 1
1368	.L30:
1369		je .L28
1370		# Stack pointer unchanged, no stack allocated arguments
1371		lea rax, [rip + .L.str13]
1372		push rax
1373		pop rax
1374		mov rax, rax
1375		mov rdi, rax
1376		# statement primitives do not return results (but will clobber r0/rax)
1377		call _prints
1378		# Stack pointer unchanged, no stack allocated arguments
1379		mov rax, r14
1380		mov rdi, rax
1381		# statement primitives do not return results (but will clobber r0/rax)
1382		call _printc
1383		# Stack pointer unchanged, no stack allocated arguments
1384		lea rax, [rip + .L.str14]
1385		push rax
1386		pop rax
1387		mov rax, rax
1388		mov rdi, rax
1389		# statement primitives do not return results (but will clobber r0/rax)
1390		call _prints
1391		call _println
1392		jmp .L29
1393	.L28:
1394		mov rax, r14
1395		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1396		mov rsp, rbp
1397		# pop {r12, r13, r14, r15}
1398		mov r12, qword ptr [rsp]
1399		mov r13, qword ptr [rsp + 8]
1400		mov r14, qword ptr [rsp + 16]
1401		mov r15, qword ptr [rsp + 24]
1402		add rsp, 32
1403		pop rbp
1404		ret
1405	.L29:
1406		# Stack pointer unchanged, no stack allocated variables
1407	.L26:
1408		mov rax, 1
1409		cmp rax, 1
1410		je .L27
1411		mov rax, 0
1412		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1413		mov rsp, rbp
1414		# pop {r12, r13, r14, r15}
1415		mov r12, qword ptr [rsp]
1416		mov r13, qword ptr [rsp + 8]
1417		mov r14, qword ptr [rsp + 16]
1418		mov r15, qword ptr [rsp + 24]
1419		add rsp, 32
1420		pop rbp
1421		ret
1422		# Stack pointer unchanged, no stack allocated variables
1423		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1424	
1425	wacc_askForInt:
1426		push rbp
1427		push r12
1428		mov rbp, rsp
1429		# Stack pointer unchanged, no stack allocated variables
1430		push rdi
1431		# Set up R11 as a temporary second base pointer for the caller saved things
1432		mov r11, rsp
1433		# Stack pointer unchanged, no stack allocated arguments
1434		mov rax, rdi
1435		mov rdi, rax
1436		# statement primitives do not return results (but will clobber r0/rax)
1437		call _prints
1438		pop rdi
1439		mov rax, 0
1440		mov r12, rax
1441		push rdi
1442		# Set up R11 as a temporary second base pointer for the caller saved things
1443		mov r11, rsp
1444		# Stack pointer unchanged, no stack allocated arguments
1445		# load the current value in the destination of the read so it supports defaults
1446		mov rax, r12
1447		mov rdi, rax
1448		call _readi
1449		mov r11, rax
1450		pop rdi
1451		mov rax, r11
1452		mov r12, rax
1453		push rdi
1454		# Set up R11 as a temporary second base pointer for the caller saved things
1455		mov r11, rsp
1456		# Stack pointer unchanged, no stack allocated arguments
1457		lea rax, [rip + .L.str13]
1458		push rax
1459		pop rax
1460		mov rax, rax
1461		mov rdi, rax
1462		# statement primitives do not return results (but will clobber r0/rax)
1463		call _prints
1464		pop rdi
1465		push rdi
1466		# Set up R11 as a temporary second base pointer for the caller saved things
1467		mov r11, rsp
1468		# Stack pointer unchanged, no stack allocated arguments
1469		mov rax, r12
1470		mov rdi, rax
1471		# statement primitives do not return results (but will clobber r0/rax)
1472		call _printi
1473		call _println
1474		pop rdi
1475		mov rax, r12
1476		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1477		mov rsp, rbp
1478		pop r12
1479		pop rbp
1480		ret
1481		# Stack pointer unchanged, no stack allocated variables
1482		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1483	
1484	wacc_handleMenuInsert:
1485		push rbp
1486		# push {r12, r13}
1487		sub rsp, 16
1488		mov qword ptr [rsp], r12
1489		mov qword ptr [rsp + 8], r13
1490		mov rbp, rsp
1491		# Stack pointer unchanged, no stack allocated variables
1492		push rdi
1493		# Set up R11 as a temporary second base pointer for the caller saved things
1494		mov r11, rsp
1495		# Stack pointer unchanged, no stack allocated arguments
1496		lea rax, [rip + .L.str15]
1497		push rax
1498		pop rax
1499		mov rax, rax
1500		mov rdi, rax
1501		call wacc_askForInt
1502		mov r11, rax
1503		# Stack pointer unchanged, no stack allocated arguments
1504		pop rdi
1505		mov rax, r11
1506		mov r12, rax
1507		push rdi
1508		# Set up R11 as a temporary second base pointer for the caller saved things
1509		mov r11, rsp
1510		# Stack pointer unchanged, no stack allocated arguments
1511		mov rax, rdi
1512		mov rdi, rax
1513		mov rax, r12
1514		mov rsi, rax
1515		call wacc_insertIfNotContain
1516		mov r11, rax
1517		# Stack pointer unchanged, no stack allocated arguments
1518		pop rdi
1519		mov rax, r11
1520		mov r13, rax
1521		cmp r13, 1
1522		je .L31
1523		push rdi
1524		# Set up R11 as a temporary second base pointer for the caller saved things
1525		mov r11, rsp
1526		# Stack pointer unchanged, no stack allocated arguments
1527		lea rax, [rip + .L.str16]
1528		push rax
1529		pop rax
1530		mov rax, rax
1531		mov rdi, rax
1532		# statement primitives do not return results (but will clobber r0/rax)
1533		call _prints
1534		call _println
1535		pop rdi
1536		jmp .L32
1537	.L31:
1538		push rdi
1539		# Set up R11 as a temporary second base pointer for the caller saved things
1540		mov r11, rsp
1541		# Stack pointer unchanged, no stack allocated arguments
1542		lea rax, [rip + .L.str17]
1543		push rax
1544		pop rax
1545		mov rax, rax
1546		mov rdi, rax
1547		# statement primitives do not return results (but will clobber r0/rax)
1548		call _prints
1549		call _println
1550		pop rdi
1551	.L32:
1552		mov rax, 1
1553		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1554		mov rsp, rbp
1555		# pop {r12, r13}
1556		mov r12, qword ptr [rsp]
1557		mov r13, qword ptr [rsp + 8]
1558		add rsp, 16
1559		pop rbp
1560		ret
1561		# Stack pointer unchanged, no stack allocated variables
1562		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1563	
1564	wacc_handleMenuFind:
1565		push rbp
1566		# push {r12, r13}
1567		sub rsp, 16
1568		mov qword ptr [rsp], r12
1569		mov qword ptr [rsp + 8], r13
1570		mov rbp, rsp
1571		# Stack pointer unchanged, no stack allocated variables
1572		push rdi
1573		# Set up R11 as a temporary second base pointer for the caller saved things
1574		mov r11, rsp
1575		# Stack pointer unchanged, no stack allocated arguments
1576		lea rax, [rip + .L.str18]
1577		push rax
1578		pop rax
1579		mov rax, rax
1580		mov rdi, rax
1581		call wacc_askForInt
1582		mov r11, rax
1583		# Stack pointer unchanged, no stack allocated arguments
1584		pop rdi
1585		mov rax, r11
1586		mov r12, rax
1587		push rdi
1588		# Set up R11 as a temporary second base pointer for the caller saved things
1589		mov r11, rsp
1590		# Stack pointer unchanged, no stack allocated arguments
1591		mov rax, rdi
1592		mov rdi, rax
1593		mov rax, r12
1594		mov rsi, rax
1595		call wacc_contain
1596		mov r11, rax
1597		# Stack pointer unchanged, no stack allocated arguments
1598		pop rdi
1599		mov rax, r11
1600		mov r13, rax
1601		cmp r13, 1
1602		je .L33
1603		push rdi
1604		# Set up R11 as a temporary second base pointer for the caller saved things
1605		mov r11, rsp
1606		# Stack pointer unchanged, no stack allocated arguments
1607		lea rax, [rip + .L.str19]
1608		push rax
1609		pop rax
1610		mov rax, rax
1611		mov rdi, rax
1612		# statement primitives do not return results (but will clobber r0/rax)
1613		call _prints
1614		call _println
1615		pop rdi
1616		jmp .L34
1617	.L33:
1618		push rdi
1619		# Set up R11 as a temporary second base pointer for the caller saved things
1620		mov r11, rsp
1621		# Stack pointer unchanged, no stack allocated arguments
1622		lea rax, [rip + .L.str20]
1623		push rax
1624		pop rax
1625		mov rax, rax
1626		mov rdi, rax
1627		# statement primitives do not return results (but will clobber r0/rax)
1628		call _prints
1629		call _println
1630		pop rdi
1631	.L34:
1632		mov rax, 1
1633		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1634		mov rsp, rbp
1635		# pop {r12, r13}
1636		mov r12, qword ptr [rsp]
1637		mov r13, qword ptr [rsp + 8]
1638		add rsp, 16
1639		pop rbp
1640		ret
1641		# Stack pointer unchanged, no stack allocated variables
1642		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1643	
1644	wacc_handleMenuCount:
1645		push rbp
1646		push r12
1647		mov rbp, rsp
1648		# Stack pointer unchanged, no stack allocated variables
1649		push rdi
1650		# Set up R11 as a temporary second base pointer for the caller saved things
1651		mov r11, rsp
1652		# Stack pointer unchanged, no stack allocated arguments
1653		mov rax, rdi
1654		mov rdi, rax
1655		call wacc_count
1656		mov r11, rax
1657		# Stack pointer unchanged, no stack allocated arguments
1658		pop rdi
1659		mov rax, r11
1660		mov r12, rax
1661		cmp r12, 1
1662		je .L35
1663		push rdi
1664		# Set up R11 as a temporary second base pointer for the caller saved things
1665		mov r11, rsp
1666		# Stack pointer unchanged, no stack allocated arguments
1667		lea rax, [rip + .L.str21]
1668		push rax
1669		pop rax
1670		mov rax, rax
1671		mov rdi, rax
1672		# statement primitives do not return results (but will clobber r0/rax)
1673		call _prints
1674		pop rdi
1675		push rdi
1676		# Set up R11 as a temporary second base pointer for the caller saved things
1677		mov r11, rsp
1678		# Stack pointer unchanged, no stack allocated arguments
1679		mov rax, r12
1680		mov rdi, rax
1681		# statement primitives do not return results (but will clobber r0/rax)
1682		call _printi
1683		pop rdi
1684		push rdi
1685		# Set up R11 as a temporary second base pointer for the caller saved things
1686		mov r11, rsp
1687		# Stack pointer unchanged, no stack allocated arguments
1688		lea rax, [rip + .L.str22]
1689		push rax
1690		pop rax
1691		mov rax, rax
1692		mov rdi, rax
1693		# statement primitives do not return results (but will clobber r0/rax)
1694		call _prints
1695		call _println
1696		pop rdi
1697		jmp .L36
1698	.L35:
1699		push rdi
1700		# Set up R11 as a temporary second base pointer for the caller saved things
1701		mov r11, rsp
1702		# Stack pointer unchanged, no stack allocated arguments
1703		lea rax, [rip + .L.str23]
1704		push rax
1705		pop rax
1706		mov rax, rax
1707		mov rdi, rax
1708		# statement primitives do not return results (but will clobber r0/rax)
1709		call _prints
1710		call _println
1711		pop rdi
1712	.L36:
1713		mov rax, 1
1714		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1715		mov rsp, rbp
1716		pop r12
1717		pop rbp
1718		ret
1719		# Stack pointer unchanged, no stack allocated variables
1720		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1721	
1722	wacc_handleMenuPrint:
1723		push rbp
1724		push r12
1725		mov rbp, rsp
1726		# Stack pointer unchanged, no stack allocated variables
1727		push rdi
1728		# Set up R11 as a temporary second base pointer for the caller saved things
1729		mov r11, rsp
1730		# Stack pointer unchanged, no stack allocated arguments
1731		lea rax, [rip + .L.str24]
1732		push rax
1733		pop rax
1734		mov rax, rax
1735		mov rdi, rax
1736		# statement primitives do not return results (but will clobber r0/rax)
1737		call _prints
1738		pop rdi
1739		push rdi
1740		# Set up R11 as a temporary second base pointer for the caller saved things
1741		mov r11, rsp
1742		# Stack pointer unchanged, no stack allocated arguments
1743		mov rax, rdi
1744		mov rdi, rax
1745		call wacc_printAll
1746		mov r11, rax
1747		# Stack pointer unchanged, no stack allocated arguments
1748		pop rdi
1749		mov rax, r11
1750		mov r12, rax
1751		mov rax, 1
1752		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1753		mov rsp, rbp
1754		pop r12
1755		pop rbp
1756		ret
1757		# Stack pointer unchanged, no stack allocated variables
1758		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1759	
1760	wacc_handleMenuRemove:
1761		push rbp
1762		# push {r12, r13}
1763		sub rsp, 16
1764		mov qword ptr [rsp], r12
1765		mov qword ptr [rsp + 8], r13
1766		mov rbp, rsp
1767		# Stack pointer unchanged, no stack allocated variables
1768		push rdi
1769		# Set up R11 as a temporary second base pointer for the caller saved things
1770		mov r11, rsp
1771		# Stack pointer unchanged, no stack allocated arguments
1772		lea rax, [rip + .L.str25]
1773		push rax
1774		pop rax
1775		mov rax, rax
1776		mov rdi, rax
1777		call wacc_askForInt
1778		mov r11, rax
1779		# Stack pointer unchanged, no stack allocated arguments
1780		pop rdi
1781		mov rax, r11
1782		mov r12, rax
1783		push rdi
1784		# Set up R11 as a temporary second base pointer for the caller saved things
1785		mov r11, rsp
1786		# Stack pointer unchanged, no stack allocated arguments
1787		mov rax, rdi
1788		mov rdi, rax
1789		mov rax, r12
1790		mov rsi, rax
1791		call wacc_remove
1792		mov r11, rax
1793		# Stack pointer unchanged, no stack allocated arguments
1794		pop rdi
1795		mov rax, r11
1796		mov r13, rax
1797		cmp r13, 1
1798		je .L37
1799		push rdi
1800		# Set up R11 as a temporary second base pointer for the caller saved things
1801		mov r11, rsp
1802		# Stack pointer unchanged, no stack allocated arguments
1803		lea rax, [rip + .L.str19]
1804		push rax
1805		pop rax
1806		mov rax, rax
1807		mov rdi, rax
1808		# statement primitives do not return results (but will clobber r0/rax)
1809		call _prints
1810		call _println
1811		pop rdi
1812		jmp .L38
1813	.L37:
1814		push rdi
1815		# Set up R11 as a temporary second base pointer for the caller saved things
1816		mov r11, rsp
1817		# Stack pointer unchanged, no stack allocated arguments
1818		lea rax, [rip + .L.str26]
1819		push rax
1820		pop rax
1821		mov rax, rax
1822		mov rdi, rax
1823		# statement primitives do not return results (but will clobber r0/rax)
1824		call _prints
1825		call _println
1826		pop rdi
1827	.L38:
1828		mov rax, 1
1829		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1830		mov rsp, rbp
1831		# pop {r12, r13}
1832		mov r12, qword ptr [rsp]
1833		mov r13, qword ptr [rsp + 8]
1834		add rsp, 16
1835		pop rbp
1836		ret
1837		# Stack pointer unchanged, no stack allocated variables
1838		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1839	
1840	wacc_handleMenuRemoveAll:
1841		push rbp
1842		push r12
1843		mov rbp, rsp
1844		# Stack pointer unchanged, no stack allocated variables
1845		push rdi
1846		# Set up R11 as a temporary second base pointer for the caller saved things
1847		mov r11, rsp
1848		# Stack pointer unchanged, no stack allocated arguments
1849		mov rax, rdi
1850		mov rdi, rax
1851		call wacc_removeAll
1852		mov r11, rax
1853		# Stack pointer unchanged, no stack allocated arguments
1854		pop rdi
1855		mov rax, r11
1856		mov r12, rax
1857		push rdi
1858		# Set up R11 as a temporary second base pointer for the caller saved things
1859		mov r11, rsp
1860		# Stack pointer unchanged, no stack allocated arguments
1861		lea rax, [rip + .L.str27]
1862		push rax
1863		pop rax
1864		mov rax, rax
1865		mov rdi, rax
1866		# statement primitives do not return results (but will clobber r0/rax)
1867		call _prints
1868		call _println
1869		pop rdi
1870		mov rax, 1
1871		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
1872		mov rsp, rbp
1873		pop r12
1874		pop rbp
1875		ret
1876		# Stack pointer unchanged, no stack allocated variables
1877		# 'ere be dragons: this is 100% dead code, functions always end in returns!
1878	
1879	_freepair:
1880		push rbp
1881		mov rbp, rsp
1882		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1883		and rsp, -16
1884		cmp rdi, 0
1885		je _errNull
1886		call free@plt
1887		mov rsp, rbp
1888		pop rbp
1889		ret
1890	
1891	.section .rodata
1892	# length of .L._readc_str0
1893		.int 3
1894	.L._readc_str0:
1895		.asciz " %c"
1896	.text
1897	_readc:
1898		push rbp
1899		mov rbp, rsp
1900		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1901		and rsp, -16
1902		# RDI contains the "original" value of the destination of the read
1903		# allocate space on the stack to store the read: preserve alignment!
1904		# the passed default argument should be stored in case of EOF
1905		sub rsp, 16
1906		mov byte ptr [rsp], dil
1907		lea rsi, qword ptr [rsp]
1908		lea rdi, [rip + .L._readc_str0]
1909		# on x86, al represents the number of SIMD registers used as variadic arguments
1910		mov al, 0
1911		call scanf@plt
1912		movsx rax, byte ptr [rsp]
1913		add rsp, 16
1914		mov rsp, rbp
1915		pop rbp
1916		ret
1917	
1918	.section .rodata
1919	# length of .L._prints_str0
1920		.int 4
1921	.L._prints_str0:
1922		.asciz "%.*s"
1923	.text
1924	_prints:
1925		push rbp
1926		mov rbp, rsp
1927		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1928		and rsp, -16
1929		mov rdx, rdi
1930		mov esi, dword ptr [rdi - 4]
1931		lea rdi, [rip + .L._prints_str0]
1932		# on x86, al represents the number of SIMD registers used as variadic arguments
1933		mov al, 0
1934		call printf@plt
1935		mov rdi, 0
1936		call fflush@plt
1937		mov rsp, rbp
1938		pop rbp
1939		ret
1940	
1941	.section .rodata
1942	# length of .L._readi_str0
1943		.int 2
1944	.L._readi_str0:
1945		.asciz "%d"
1946	.text
1947	_readi:
1948		push rbp
1949		mov rbp, rsp
1950		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1951		and rsp, -16
1952		# RDI contains the "original" value of the destination of the read
1953		# allocate space on the stack to store the read: preserve alignment!
1954		# the passed default argument should be stored in case of EOF
1955		sub rsp, 16
1956		mov dword ptr [rsp], edi
1957		lea rsi, qword ptr [rsp]
1958		lea rdi, [rip + .L._readi_str0]
1959		# on x86, al represents the number of SIMD registers used as variadic arguments
1960		mov al, 0
1961		call scanf@plt
1962		movsx rax, dword ptr [rsp]
1963		add rsp, 16
1964		mov rsp, rbp
1965		pop rbp
1966		ret
1967	
1968	_malloc:
1969		push rbp
1970		mov rbp, rsp
1971		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1972		and rsp, -16
1973		call malloc@plt
1974		cmp rax, 0
1975		je _errOutOfMemory
1976		mov rsp, rbp
1977		pop rbp
1978		ret
1979	
1980	.section .rodata
1981	# length of .L._printi_str0
1982		.int 2
1983	.L._printi_str0:
1984		.asciz "%d"
1985	.text
1986	_printi:
1987		push rbp
1988		mov rbp, rsp
1989		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1990		and rsp, -16
1991		mov esi, edi
1992		lea rdi, [rip + .L._printi_str0]
1993		# on x86, al represents the number of SIMD registers used as variadic arguments
1994		mov al, 0
1995		call printf@plt
1996		mov rdi, 0
1997		call fflush@plt
1998		mov rsp, rbp
1999		pop rbp
2000		ret
2001	
2002	.section .rodata
2003	# length of .L._printc_str0
2004		.int 2
2005	.L._printc_str0:
2006		.asciz "%c"
2007	.text
2008	_printc:
2009		push rbp
2010		mov rbp, rsp
2011		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2012		and rsp, -16
2013		mov sil, dil
2014		lea rdi, [rip + .L._printc_str0]
2015		# on x86, al represents the number of SIMD registers used as variadic arguments
2016		mov al, 0
2017		call printf@plt
2018		mov rdi, 0
2019		call fflush@plt
2020		mov rsp, rbp
2021		pop rbp
2022		ret
2023	
2024	.section .rodata
2025	# length of .L._println_str0
2026		.int 0
2027	.L._println_str0:
2028		.asciz ""
2029	.text
2030	_println:
2031		push rbp
2032		mov rbp, rsp
2033		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2034		and rsp, -16
2035		lea rdi, [rip + .L._println_str0]
2036		call puts@plt
2037		mov rdi, 0
2038		call fflush@plt
2039		mov rsp, rbp
2040		pop rbp
2041		ret
2042	
2043	_exit:
2044		push rbp
2045		mov rbp, rsp
2046		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2047		and rsp, -16
2048		call exit@plt
2049		mov rsp, rbp
2050		pop rbp
2051		ret
2052	
2053	_arrLoad8:
2054		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
2055		push rbx
2056		cmp r10d, 0
2057		cmovl esi, r10d
2058		jl _errOutOfBounds
2059		mov ebx, dword ptr [r9 - 4]
2060		cmp r10d, ebx
2061		cmovge esi, r10d
2062		jge _errOutOfBounds
2063		mov r9, qword ptr [r9 + 8*r10]
2064		pop rbx
2065		ret
2066	
2067	_arrStore8:
2068		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
2069		push rbx
2070		cmp r10d, 0
2071		cmovl esi, r10d
2072		jl _errOutOfBounds
2073		mov ebx, dword ptr [r9 - 4]
2074		cmp r10d, ebx
2075		cmovge esi, r10d
2076		jge _errOutOfBounds
2077		mov qword ptr [r9 + 8*r10], rax
2078		pop rbx
2079		ret
2080	
2081	.section .rodata
2082	# length of .L._errOutOfMemory_str0
2083		.int 27
2084	.L._errOutOfMemory_str0:
2085		.asciz "fatal error: out of memory\n"
2086	.text
2087	_errOutOfMemory:
2088		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2089		and rsp, -16
2090		lea rdi, [rip + .L._errOutOfMemory_str0]
2091		call _prints
2092		mov dil, -1
2093		call exit@plt
2094	
2095	.section .rodata
2096	# length of .L._errDivZero_str0
2097		.int 40
2098	.L._errDivZero_str0:
2099		.asciz "fatal error: division or modulo by zero\n"
2100	.text
2101	_errDivZero:
2102		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2103		and rsp, -16
2104		lea rdi, [rip + .L._errDivZero_str0]
2105		call _prints
2106		mov dil, -1
2107		call exit@plt
2108	
2109	.section .rodata
2110	# length of .L._errNull_str0
2111		.int 45
2112	.L._errNull_str0:
2113		.asciz "fatal error: null pair dereferenced or freed\n"
2114	.text
2115	_errNull:
2116		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2117		and rsp, -16
2118		lea rdi, [rip + .L._errNull_str0]
2119		call _prints
2120		mov dil, -1
2121		call exit@plt
2122	
2123	.section .rodata
2124	# length of .L._errOutOfBounds_str0
2125		.int 42
2126	.L._errOutOfBounds_str0:
2127		.asciz "fatal error: array index %d out of bounds\n"
2128	.text
2129	_errOutOfBounds:
2130		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2131		and rsp, -16
2132		lea rdi, [rip + .L._errOutOfBounds_str0]
2133		# on x86, al represents the number of SIMD registers used as variadic arguments
2134		mov al, 0
2135		call printf@plt
2136		mov rdi, 0
2137		call fflush@plt
2138		mov dil, -1
2139		call exit@plt
2140	
2141	.section .rodata
2142	# length of .L._errOverflow_str0
2143		.int 52
2144	.L._errOverflow_str0:
2145		.asciz "fatal error: integer overflow or underflow occurred\n"
2146	.text
2147	_errOverflow:
2148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2149		and rsp, -16
2150		lea rdi, [rip + .L._errOverflow_str0]
2151		call _prints
2152		mov dil, -1
2153		call exit@plt
===========================================================
-- Finished

