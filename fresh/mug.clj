; util
(defn escape-str [s]
	(apply str (map (fn [c] (or (char-escape-string c) c)) s)))
(defn flatten [x]
	(filter (complement sequential?)
		(rest (tree-seq sequential? seq x))))
(defn join [x y]
	(apply str (interpose x y)))

; ast

;TODO defstruct
(defmacro defast [type parent types]
	(do
		(derive type parent)
		(let [keywords (conj (map keyword (map name (filter (fn [x] (not= '& x)) types))) :type)
			symbols (conj (filter (fn [x] (not= '& x)) types) type)
			ast-struct (symbol (str "ast-" (name type)))]
			(eval (concat (list 'defstruct ast-struct) keywords))
			(eval (list 'defn (symbol (name type)) types
				(list 'apply 'struct (list 'cons ast-struct (vec symbols))))))))

; literals
(derive ::literal ::expr)
(defast ::num-literal ::literal [value])
(defast ::str-literal ::literal [value])
(defast ::obj-literal ::literal [map])
(defast ::array-literal ::literal [exprs])
(defast ::undef-literal ::literal [])
(defast ::null-literal ::literal [])

; operations
(derive ::op-expr ::expr)
(derive ::unary-op-expr ::op-expr)
	(defn operand [n] (:expr n))
(derive ::binary-op-expr ::op-expr)
	(defn left-operand [n] (:left n))
	(defn right-operand [n] (:right n))
(defast ::lt-op-expr ::binary-op-expr [left right])
(defast ::lte-op-expr ::binary-op-expr [left right])
(defast ::gt-op-expr ::binary-op-expr [left right])
(defast ::gte-op-expr ::binary-op-expr [left right])
(defast ::add-op-expr ::binary-op-expr [left right])
(defast ::sub-op-expr ::binary-op-expr [left right])
(defast ::mul-op-expr ::binary-op-expr [left right])
(defast ::div-op-expr ::binary-op-expr [left right])
(defast ::mod-op-expr ::binary-op-expr [left right])
(defast ::neq-op-expr ::binary-op-expr [left right])
(defast ::eq-op-expr ::binary-op-expr [left right])
(defast ::neqs-op-expr ::binary-op-expr [left right])
(defast ::eqs-op-expr ::binary-op-expr [left right])

(defast ::closure ::ast-node [vars & stats])

; expressions
(derive ::expr ::ast-node)
(defast ::stats-expr ::expr [stats expr])
(defast ::this-expr ::expr [])
(defast ::scope-ref-expr ::expr [value])
(defast ::static-ref-expr ::expr [base value])
(defast ::dyn-ref-expr ::expr [base index])
(defast ::static-method-call-expr ::expr [base value & args])
(defast ::dyn-method-call-expr ::expr [base index & args])
(defast ::new-expr ::expr [constructor & args])
(defast ::call-expr ::expr [ref & args])
(defast ::scope-assign-expr ::expr [value expr])
(defast ::static-assign-expr ::expr [base value expr])
(defast ::dyn-assign-expr ::expr [base index expr])
(defast ::typeof-expr ::expr [ref])
(defast ::if-expr ::expr [expr then-expr else-expr])
(defast ::func-expr ::expr [name args closure])
(defast ::class-expr ::expr [name prototype constructor static])
	(defn class-name [n] (:name n))
	(defn class-static [n] (:static n))
	(defn class-prototype [n] (:prototype n))
(defast ::constructor ::ast-node [args closure])

; statements
(derive ::stat ::ast-node)
(defast ::block-stat ::stat [& stats])
(defast ::if-stat ::stat [expr then-stat else-stat])
(defast ::class-stat ::stat [name prototype constructor static])
(defast ::ret-stat ::stat [expr])
(defast ::while-stat ::stat [expr stat])
(defast ::for-in-stat ::stat [value from to by stat])
(defast ::expr-stat ::stat [expr])

; a sample AST
(def person (closure #{"ITER", "Chain", "Person", "chain", "start", "end", "i"}
	(class-stat "Person" {
		"count" (num-literal 0)
		"prev" (null-literal)
		"next" (null-literal)
		"shout" (func-expr nil ["shout", "deadif"] (closure #{}
			(if-stat (lt-op-expr (scope-ref-expr "shout") (scope-ref-expr "deadif"))
				(ret-stat (add-op-expr (scope-ref-expr "shout") (num-literal 1))) nil)
			(expr-stat (static-assign-expr (static-ref-expr (this-expr) "prev") "next" (static-ref-expr (this-expr) "next")))
			(expr-stat (static-assign-expr (static-ref-expr (this-expr) "next") "prev" (static-ref-expr (this-expr) "prev")))
			(ret-stat (num-literal 1))
		))}
		(constructor ["count"] (closure #{}
			(expr-stat (static-assign-expr (this-expr) "count" (scope-ref-expr "count")))))
		nil)
	(class-stat "Chain" {
		"first" (null-literal)
		"kill" (func-expr nil ["nth"] (closure #{"current" "shout"}
			(expr-stat (scope-assign-expr "current" (static-ref-expr (this-expr) "first")))
			(expr-stat (scope-assign-expr "shout" (num-literal 1)))
			(while-stat (neqs-op-expr (static-ref-expr (scope-ref-expr "current") "next") (scope-ref-expr "current")) (block-stat
				(expr-stat (scope-assign-expr "shout" (static-method-call-expr (scope-ref-expr "current") "shout" (scope-ref-expr "shout") (scope-ref-expr "nth"))))
				(expr-stat (scope-assign-expr "current" (static-ref-expr (scope-ref-expr "current") "next")))))
			(expr-stat (static-assign-expr (this-expr) "first" (scope-ref-expr "current")))
			(ret-stat (scope-ref-expr "current"))
		))}
		(constructor ["size"] (closure #{"last" "current"}
			(expr-stat (scope-assign-expr "last" (null-literal)))
			(expr-stat (scope-assign-expr "current" (null-literal)))
			(for-in-stat "i" (num-literal 0) (scope-ref-expr "size") (num-literal 1) (block-stat
				(expr-stat (scope-assign-expr "current" (new-expr (scope-ref-expr "Person") (scope-ref-expr "i"))))
				(if-stat (eqs-op-expr (static-ref-expr (this-expr) "first") (null-literal))
					(expr-stat (static-assign-expr (this-expr) "first" (scope-ref-expr "current"))) nil)
				(if-stat (neqs-op-expr (scope-ref-expr "last") (null-literal)) (block-stat
					(expr-stat (static-assign-expr (scope-ref-expr "last") "next" (scope-ref-expr "current")))
					(expr-stat (static-assign-expr (scope-ref-expr "current") "prev" (scope-ref-expr "last")))) nil)
				(expr-stat (scope-assign-expr "last" (scope-ref-expr "current")))))
			(expr-stat (static-assign-expr (static-ref-expr (this-expr) "first") "prev" (scope-ref-expr "last")))
			(expr-stat (static-assign-expr (scope-ref-expr "last") "next" (static-ref-expr (this-expr) "first")))))
		nil)
	(expr-stat (call-expr (scope-ref-expr "print") (str-literal "Starting.")))
	(expr-stat (scope-assign-expr "start" (call-expr (scope-ref-expr "nanoTime"))))
	(expr-stat (scope-assign-expr "ITER" (num-literal 500000)))
	(for-in-stat "i" (num-literal 0) (scope-ref-expr "ITER") (num-literal 1) (block-stat
		(expr-stat (scope-assign-expr "chain" (new-expr (scope-ref-expr "Chain") (num-literal 40))))
		(expr-stat (static-method-call-expr (scope-ref-expr "chain") "kill" (num-literal 3)))))
	(expr-stat (call-expr (scope-ref-expr "print") (str-literal "Ended.")))
	(expr-stat (scope-assign-expr "end" (call-expr (scope-ref-expr "nanoTime"))))
	(expr-stat (call-expr (scope-ref-expr "print") (add-op-expr (add-op-expr (str-literal "Time per iteration = ") (div-op-expr (sub-op-expr (scope-ref-expr "end") (scope-ref-expr "start")) (scope-ref-expr "ITER"))) (str-literal " nanoseconds."))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compile javascript from AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-js-ctx [] {:scopes [] :tab 0})
(defn js-ctx-incr-tab [c]
	(merge c {:tab (+ (:tab c) 1) }))
(defn js-ctx-tab [c]
	(apply str (repeat (:tab c) \tab)))
(defn js-ctx-push-scope [c s]
	(merge c {:scopes (cons s (:scopes c))}))

(defn js-ctx-temp-var [c i]
	(nth (filter (fn [k] (not (contains? (first (:scopes c)) k))) 
		((fn tmp-var [a] 
		   (cons a (lazy-seq (tmp-var (str "_tmp" (+ 1 (Integer/parseInt (.substring a 4))))))))
		"_tmp0")) i))
(defn js-ctx-define-vars [c & v]
	(merge c {:scopes (cons
		(set (concat (first (:scopes c)) v))
		(rest (:scopes c)))}))

(defmulti js-compile-type (fn [& args] (:type (first args))))
(defn js-compile [x]
	(js-compile-type x (new-js-ctx)))
(defmethod js-compile-type ::ast-node [n c]) ; catch-all

; literals
(defmethod js-compile-type ::num-literal [n c]
	(str (:value n)))
(defmethod js-compile-type ::null-literal [n c]
	"null")
(defmethod js-compile-type ::str-literal [n c]
	(str \" (escape-str (:value n)) \"))

; exprs
(defmethod js-compile-type ::this-expr [n c]
	"this")
(defmethod js-compile-type ::scope-ref-expr [n c]
	(str (:value n)))
(defmethod js-compile-type ::static-ref-expr [n c]
	(str (js-compile-type (:base n) c) "." (:value n)))
(defmethod js-compile-type ::dyn-ref-expr [n c]
	(str (js-compile-type (:base n) c) "[" (js-compile-type (:index n) c) "]"))
(defmethod js-compile-type ::static-method-call-expr [n c]
	(str (js-compile-type (:base n) c) "." (:value n) "(" (join ", " (map (fn [x] (js-compile-type x c)) (:args n))) ")"))
(defmethod js-compile-type ::scope-assign-expr [n c]
	(str (:value n) " = " (js-compile-type (:expr n) c)))
(defmethod js-compile-type ::static-assign-expr [n c]
	(str (js-compile-type (:base n) c) "." (:value n) " = " (js-compile-type (:expr n) c)))
(defmethod js-compile-type ::func-expr [n c]
	(str "function " (or (:name n) "") "(" (join ", " (:args n)) ")\n" (js-compile-type (:closure n) c)))
(defmethod js-compile-type ::call-expr [n c]
	(str (js-compile-type (:ref n) c) "(" (join ", " (map (fn [x] (js-compile-type x c)) (:args n))) ")"))
(defmethod js-compile-type ::new-expr [n c]
	(str "new " (js-compile-type (:constructor n) c) "(" (join ", " (map (fn [x] (js-compile-type x c)) (:args n))) ")"))

; misc
(defmethod js-compile-type ::closure [n c] 
	(str (js-ctx-tab c) "{\n"
		(apply str (if (not (empty? (:vars n)))
			[(js-ctx-tab (js-ctx-incr-tab c)) "var " (join ", " (:vars n)) ";\n"]
			[]))
		(apply str (map (fn [n] (js-compile-type n c)) (:stats n)))
		(js-ctx-tab c) "}"))

; stats
(defmethod js-compile-type ::block [n c] 
	(str (js-ctx-tab c) "{\n"
		(apply str (map (fn [n] (js-compile-type n c)) (:stats n)))
		(js-ctx-tab c) "}"))
(defmethod js-compile-type ::block-stat [n c]
	(str (js-compile-type {:type ::block :vars (:vars n) :stats (:stats n)} c) ";\n"))
(defmethod js-compile-type ::class-stat [n c] (let [c (js-ctx-incr-tab c)] (str
	(if (nil? (:constructor n))
		(str (js-ctx-tab c) (class-name n) " = function () { }\n")
		(str (js-ctx-tab c) (class-name n) " = function (" (join ", " (:args (:constructor n))) ")\n" (js-compile-type (:closure (:constructor n)) c) ";\n"))
	(apply str (map (fn [[k v]] (str (js-ctx-tab c) (class-name n) ".prototype." k " = "
		(js-compile-type v c) ";\n")) (class-prototype n))))))
(defmethod js-compile-type ::if-stat [n c] (let [c (js-ctx-incr-tab c)]
	(str (js-ctx-tab c) "if (" (js-compile-type (:expr n) c) ")\n"
		(js-compile-type (:then-stat n) c)
		(if (contains? n :else-expr) (str "else\n"
			(js-compile-type (:else-stat n) c)) ""))))
(defmethod js-compile-type ::ret-stat [n c] (let [c (js-ctx-incr-tab c)]
	(str (js-ctx-tab c) "return" (if (contains? n :expr) (str " " (js-compile-type (:expr n) c)) "") ";\n")))
(defmethod js-compile-type ::expr-stat [n c] (let [c (js-ctx-incr-tab c)] 
	(str (js-ctx-tab c) (js-compile-type (:expr n) c) ";\n")))
(defmethod js-compile-type ::while-stat [n c] (let [c (js-ctx-incr-tab c)]
	(str (js-ctx-tab c) "while (" (js-compile-type (:expr n) c) ")\n"
		(js-compile-type (:stat n) c))))
(defmethod js-compile-type ::for-in-stat [n c] (let [c (js-ctx-incr-tab c)]
	(let [_from (js-ctx-temp-var c 0)
		_to (js-ctx-temp-var c 1)
		c (js-ctx-define-vars c _from _to)]
		(str (js-ctx-tab c) "var " _from " = " (js-compile-type (:from n) c) ", " _to " = " (js-compile-type (:to n) c) ";\n"
			(js-ctx-tab c) "for (" (:value n) " = " _from "; "
			_from " < " _to " ? " (:value n) " < " _to " : " (:value n) " > " _to "; "
			(:value n) " += (" _from " < " _to " ? " (js-compile-type (:by n) c) " : -(" (js-compile-type (:by n) c) ")))\n"
			(js-compile-type (:stat n) c)))))

; operations
(def binary-op-char
	{::lt-op-expr "<"
	::lte-op-expr "<="
	::gt-op-expr ">"
	::gte-op-expr ">="
	::add-op-expr "+"
	::sub-op-expr "-"
	::mul-op-expr "*"
	::div-op-expr "/"
	::mod-op-expr "%"
	::eq-op-expr "=="
	::eqs-op-expr "==="
	::neq-op-expr "!="
	::neqs-op-expr "!=="})
(defmethod js-compile-type ::binary-op-expr [n c]
	(str \( (js-compile-type (left-operand n) c) \space (binary-op-char (:type n)) \space (js-compile-type (right-operand n) c) \)))

(print (js-compile person))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compile java from AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import '(org.objectweb.asm ClassWriter Opcodes))
(import 'java.io.FileOutputStream)

(defn write-file [path bytes]
	(let [fos (new FileOutputStream path)]
		(.write fos bytes)
		(.close fos)))

(defn map-indexed [callback coll]
	(map (fn [[k v]] (callback k v)) (map vector (iterate inc 0) coll)))

; traverse tree
; define scope variables
; define number literals, string literals
; define structs
; define accessor shortcuts

; you'll want to keep a running local-variable counter for current scope
; numbers, strings, booleans, etc. are all immutable, and should be defined as properties of current method

; define a new baseline AST

(def test-ast (closure #{"print"}
	(expr-stat (call-expr (scope-ref-expr "print") (str-literal "Starting.")))
	))

;
; context manipulation	
;

(defn new-asm-ctx [] {
	:closures [] :structs [] :numbers {} :strings {} :accessors {} ; analysis
	:scopes [] ; used for analysis compilation
})

(defn asm-ctx-set [k v ctx]
	(if (not (contains? k ctx)) (merge ctx {k (assoc (ctx k) v (count (ctx k)))}) ctx))

(defn asm-ctx-get [k ctx]
	(first (ctx k)))
(defn asm-ctx-push [k v ctx]
	(merge ctx {k (cons v (ctx k))}))
(defn asm-ctx-pop [k ctx]
	(merge ctx {k (rest (ctx k))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; analysis
;

(defmulti asm-analyze-ast (fn [& args] (:type (first args))))
	
(defn asm-analyze-ast-coll [coll ctx]
	(if (first coll)
		(loop [ctx (asm-analyze-ast (first coll) ctx) coll (next coll)]
			(if coll
				(recur (asm-analyze-ast (first coll) ctx) (next coll))
				ctx))
		ctx))
		
;
; literals
;

(defmethod asm-analyze-ast ::num-literal [node ctx]
	(asm-ctx-set :numbers (node :value) ctx))

(defmethod asm-analyze-ast ::str-literal [node ctx]
	(asm-ctx-set :strings (node :value) ctx))

;
; expressions
;

(defmethod asm-analyze-ast ::scope-ref-expr [node ctx]
	ctx)
	
(defmethod asm-analyze-ast ::call-expr [node ctx]
	(->> ctx
		(asm-analyze-ast (node :ref) ,,,)
		(asm-analyze-ast-coll (node :args) ,,,)))

;
; statements
;

(defmethod asm-analyze-ast ::expr-stat [node ctx]
	(asm-analyze-ast (node :expr) ctx))

;
; closure
;

(defmethod asm-analyze-ast ::closure [node ctx]
	(->> ctx
		(asm-ctx-push :closures {:node node :scopes (ctx :scopes)} ,,,)
		(asm-ctx-push :scopes (dec (count (ctx :closures))) ,,,)
		(asm-analyze-ast-coll (node :stats) ,,,)
		(asm-ctx-pop :scopes ,,,)))
	
;;;TEST

(asm-analyze-ast test-ast (new-asm-ctx))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ast-compilation
;

;
; literals
;

(defmulti asm-compile-closure-ast (fn [& args] (:type (first args))))

(defmethod asm-compile-closure-ast ::num-literal [node closure ctx mw]
	(.visitFieldInsn mw Opcodes/GETSTATIC, "mug/compiled/JSConstants" (str "NUM_" ((ctx :numbers) (node :value))) "Lmug/compiled/JSNumber;"))

(defmethod asm-compile-closure-ast ::str-literal [node closure ctx mw]
	(.visitFieldInsn mw Opcodes/GETSTATIC "mug/compiled/JSConstants" (str "STR_" ((ctx :strings) (node :value))) "Lmug/compiled/JSString;"))
	
(defmethod asm-compile-closure-ast ::scope-ref-expr [node closure ctx mw]
	(.visitInsn mw Opcodes/ALOAD, 0)
	(.visitMethodInsn mw Opcodes/INVOKEVIRTUAL, "mug/compiled/JSScope_0", (str "get_" (node :value)), "()Lmug/JSPrimitive;"))

;
; expressions
;

(defmethod asm-compile-closure-ast ::call-expr [node closure ctx mw]
	; call expression
	(asm-compile-closure-ast (node :ref) closure ctx mw)
	; this object
	(.visitInsn mw Opcodes/ACONST_NULL)
	; arguments
	(doseq [arg (node :args)]
		(asm-compile-closure-ast arg closure ctx mw))
	; empty arguments
	(doseq [_ (repeat (- 4 (count (node :args))))]
		(.visitInsn mw Opcodes/ACONST_NULL))
	; make call
	(.visitMethodInsn mw Opcodes/INVOKEVIRTUAL, "mug/JSFunction", "invoke", "(Lmug/compiled/JSObject;Lmug/JSPrimitive;Lmug/JSPrimitive;Lmug/JSPrimitive;Lmug/JSPrimitive;)Lmug/JSPrimitive;"))

;
; statements
;

(defmethod asm-compile-closure-ast ::expr-stat [node closure ctx mw]
	(asm-compile-closure-ast (node :expr) closure ctx mw)
	(.visitInsn mw Opcodes/POP))

;
; closure
;

(defn asm-compile-closure-init [closure ctx cw]
	(doto (.visitMethod cw, 0, "<init>", "()V", nil, nil)
		(.visitCode)
		(.visitVarInsn Opcodes/ALOAD, 0)
		(.visitMethodInsn Opcodes/INVOKESPECIAL, "mug/JSFunction", "<init>", "()V")
		(.visitInsn Opcodes/RETURN)
		(.visitMaxs 1, 1)
		(.visitEnd)))
		
(defn asm-compile-closure-method [closure ctx cw]
	(let [mw (.visitMethod cw, Opcodes/ACC_PUBLIC, "invoke", "(Lmug/compiled/JSObject;Lmug/JSPrimitive;Lmug/JSPrimitive;Lmug/JSPrimitive;Lmug/JSPrimitive;)Lmug/JSPrimitive;", nil, (into-array ["java/lang/Exception"]))]
		(.visitCode mw)
		
		; initialize scope
		(.visitTypeInsn mw Opcodes/NEW "mug/Script$CSScope0")
		(.visitInsn mw Opcodes/DUP)
		(.visitMethodInsn mw Opcodes/INVOKESPECIAL, "mug/Script$CSScope0", "<init>", "()V")
		(.visitVarInsn mw Opcodes/ASTORE, 0)
		
		; compile statements
		(doseq [stat ((closure :nodes) :stats)]
			(asm-compile-closure-ast stat closure ctx mw))
		
		; just in case
		(.visitInsn mw Opcodes/RETURN)
			
		(.visitMaxs mw 6, 7)
		(.visitEnd mw)))

(defn asm-compile-closure-class [closure ctx]
	(let [name (str "mug/compiled/JSClosure_" (count (ctx :closure)))
		cw (new ClassWriter 0)]
		(.visit cw, Opcodes/V1_6, (+ Opcodes/ACC_SUPER Opcodes/ACC_PUBLIC), name, nil, "mug/JSFunction", nil)
		(asm-compile-closure-init closure ctx cw)
		(asm-compile-closure-method closure ctx cw)
		(.visitEnd cw)
		cw))

;;;TEST

(write-file "out/JSClosure_0.class"
	(.toByteArray (let [ctx (asm-analyze-ast test-ast (new-asm-ctx))]
		(asm-compile-closure-class (first (ctx :closures)) ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; constants
;

(defn asm-compile-constants-fields [strings cw]
	; undefined
	(.visitEnd (.visitField cw, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), "UNDEFINED", "Lmug/JSUndefined;", nil, nil))

	; booleans
	(.visitEnd (.visitField cw, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), "TRUE", "Lmug/JSBoolean;", nil, nil))
	(.visitEnd (.visitField cw, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), "FALSE", "Lmug/JSBoolean;", nil, nil))

	; strings
	(doseq [[_ index] strings]
			(.visitEnd (.visitField cw, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), (str "STR_" index), "Lmug/JSString;", nil, nil))))

(defn asm-compile-constants-clinit [strings cw]
	(let [mv (.visitMethod cw, Opcodes/ACC_STATIC, "<clinit>", "()V", nil, nil)]
		(.visitCode mv)

		; undefined
		(doto mv
			(.visitTypeInsn Opcodes/NEW "mug/JSUndefined")
			(.visitInsn Opcodes/DUP)
			(.visitMethodInsn Opcodes/INVOKESPECIAL, "mug/JSUndefined", "<init>", "()V")
			(.visitFieldInsn Opcodes/PUTSTATIC, "mug/compiled/JSConstants", "UNDEFINED", "Lmug/JSUndefined;"))

		; booleans
		(doto mv
			(.visitTypeInsn Opcodes/NEW "mug/JSBoolean")
			(.visitInsn Opcodes/DUP)
			(.visitInsn Opcodes/ICONST_1)
			(.visitMethodInsn Opcodes/INVOKESPECIAL, "mug/JSBoolean", "<init>", "(Z)V")
			(.visitFieldInsn Opcodes/PUTSTATIC, "mug/compiled/JSConstants", "TRUE", "Lmug/JSBoolean;"))
		(doto mv
			(.visitTypeInsn Opcodes/NEW "mug/JSBoolean")
			(.visitInsn Opcodes/DUP)
			(.visitInsn Opcodes/ICONST_0)
			(.visitMethodInsn Opcodes/INVOKESPECIAL, "mug/JSBoolean", "<init>", "(Z)V")
			(.visitFieldInsn Opcodes/PUTSTATIC, "mug/compiled/JSConstants", "FALSE", "Lmug/JSBoolean;"))

		; strings
		(doseq [[k v] strings]		
			(doto mv
				(.visitTypeInsn Opcodes/NEW "mug/JSString")
				(.visitInsn Opcodes/DUP)
				(.visitLdcInsn "Starting.")
				(.visitMethodInsn Opcodes/INVOKESPECIAL, "mug/JSString", "<init>", "(Ljava/lang/String;)V")
				(.visitFieldInsn Opcodes/PUTSTATIC, "mug/compiled/JSConstants", "STR_0", "Lmug/JSString;")))

		(doto mv
			(.visitInsn Opcodes/RETURN)
			(.visitMaxs 3, 0)
			(.visitEnd))))

(defn asm-compile-constants-class [ctx]
	(let [cw (new ClassWriter 0)
		strings (ctx :strings)]
		(.visit cw, Opcodes/V1_6, (+ Opcodes/ACC_SUPER Opcodes/ACC_PUBLIC), "mug/compiled/JSConstants", nil, "java/lang/Object", nil)
		(asm-compile-constants-fields strings cw)
		(asm-compile-constants-clinit strings cw)
		(.visitEnd cw)
		(.toByteArray cw)))

(write-file "out/JSConstants.class" (asm-compile-constants-class (asm-analyze-ast test-ast (new-asm-ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; scopes
;

(defn asm-compile-scope-fields [name scope cw]
	(doseq [var scope]
			(.visitEnd (.visitField cw, (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC), (str "_" var), "Lmug/compiled/JSPrimitive;", nil, nil))))

(defn asm-compile-scope-methods [name scope cw]
	(doseq [var scope]
		(doto (.visitMethod cw, Opcodes/ACC_PUBLIC, "get_print", "()Lmug/compiled/JSPrimitive;", nil, nil)
			(.visitCode)
			(.visitVarInsn Opcodes/ALOAD, 0)
			(.visitFieldInsn Opcodes/GETFIELD, name, (str "_" var), "Lmug/compiled/JSPrimitive;");
			(.visitInsn Opcodes/ARETURN)
			(.visitMaxs 1, 1)
			(.visitEnd))
		(doto (.visitMethod cw, Opcodes/ACC_PUBLIC, "set_print", "(Lmug/compiled/JSPrimitive;)V", nil, nil)
			(.visitCode)
			(.visitVarInsn Opcodes/ALOAD, 0)
			(.visitVarInsn Opcodes/ALOAD, 1)
			(.visitFieldInsn Opcodes/PUTFIELD, name, (str "_" var), "Lmug/compiled/JSPrimitive;");
			(.visitInsn Opcodes/RETURN)
			(.visitMaxs 2, 2)
			(.visitEnd))))

(defn asm-compile-scope-init [name scope cw]
	(doto (.visitMethod cw, 0, "<init>", "()V", nil, nil)
		(.visitCode)
		(.visitVarInsn Opcodes/ALOAD, 0)
		(.visitMethodInsn Opcodes/INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
		(.visitInsn Opcodes/RETURN)
		(.visitMaxs 1, 1)
		(.visitEnd)))

(defn asm-compile-scope-classes [ctx]
	(into {}
		(map-indexed (fn [i closure] 
			(let [cw (new ClassWriter 0)
				scope (closure :vars)
				name (str "mug/compiled/JSScope_" i)]
				(.visit cw, Opcodes/V1_6, (+ Opcodes/ACC_SUPER Opcodes/ACC_PUBLIC), name, nil, "java/lang/Object", nil)
				(asm-compile-scope-fields name scope cw)
				(asm-compile-scope-methods name scope cw)
				(asm-compile-scope-init name scope cw)
				(.visitEnd cw)
				[(str "JSScope_" i ".class") (.toByteArray cw)]))
		(reverse (ctx :closures)))))

(doseq [[path bytes] (asm-compile-scope-classes (asm-analyze-ast test-ast (new-asm-ctx)))]
	(write-file (str "out/" path) bytes))