(ns clsl.lang)
;stub of a more smybolic approach to the shader generation using spirv
;not at all working. Not even really a prototype
(defmacro defform [& args]
  (let [gen-sym 
        validator-sym (symbol (str (first args) "-validator"))
        genfn (second (next args))
        validatorfn (second (next (next ~args)))]
   `(do
    (def (symbol (str (first args) "-gen")))
    (def (symbol (str (first args) "-validator")))
    (defn ~(first args) [~'fnargs]
      {:type ~(second args)
       :symbol '~(first args)
       :genfn ~(symbol (str (first args) "-gen"))
       :validator ~(symbol (str (first args) "-validator"))
       :context {}
       :args ~'fnargs} 
      )
    )))

(defn generate-leaf-types []
 ["%f32 OpTypeFloat 32"
  "%i32 OpTypeInt 32 1"])
(defn generate-composite-types []
 ["%ivec2 OpTypeVector %i32 2"
  "%ivec3 OpTypeVector %i32 3"
  "%ivec4 OpTypeVector %i32 4"
  "%vec2 OpTypeVector %f32 2"
  "%vec3 OpTypeVector %f32 3"
  "%vec4 OpTypeVector %f32 4"])

(defn map-type [leaf]
  (cond 
    :float "%f32"
    :int "%i32"
    :ivec2 "%ivec2"
    :ivec3 "%ivec3"
    :ivec4 "%ivec4"
    :vec2 "%vec2"
    :vec3 "%vec3"
    :vec4 "%vec4"
    :else nil
    )
  )

(defn get-type "tries to infer the type of the argument. very dumb" [value] 
  (let [ez (:type value)]
    (cond
      (= ez :operator) ez ;TODO: make operator-type return type of expr
      :else ez)))

(defn inner-type "tries to infer the type of the elements of the argument. only works on colls" [value] 
  nil;stub
  )

(defn isColl [arg]
  (= (get-type arg) :coll))
(defn collsize [coll]
  (:size (:context coll)))

(defn generate [f]
  (cond
    (map? f) ((:genfn f) (:context f) (:args f))
    :else f))

(defn +-gen [context args]
  (apply str (interpose "+" (map generate args))))

(defn +-validator [context args]
  (reduce #(and %1 %2) (map get-type args)))

(defn + [& args] 
  {:type :operator
   :symbol '+
   :genfn +-gen
   :validator +-validator
   :context {}
   :args args})
(defn - [& args] 
  {:type :operator
   :symbol '-
   :genfn #(identity nil) ;dummy
   :validator #(identity nil) ;dummy
   :context {}
   :args args})
(defn * [& args] 
  {:type :operator
   :symbol '*
   :genfn #(identity nil) ;dummy
   :validator #(identity nil) ;dummy
   :context {}
   :args args})
(defn / [& args] 
  {:type :operator
   :symbol '/
   :genfn #(identity nil) ;dummy
   :validator #(identity nil) ;dummy
   :context {}
   :args args})

(defn arr [& args] 
  {:type :coll
   :symbol 'arr
   :genfn #(str "{" (clojure.string/join "," %2) "}") ;dummy
   :validator #(identity nil) ;dummy
   :context {:size (count args)}
   :args args})

(defn subscript [& args] 
  {:type :operator
   :symbol '[]
   :genfn #(str (generate (first %2)) "[" (generate (second %2)) "]") ;dummy
   :validator #(identity nil) ;dummy
   :context {}
   :args args})

(defn for-loop [loopvar start end f]
  (str "for(" loopvar "=" start ";" 
       loopvar "<" end ";" loopvar "++){" \newline
       \tab (generate f)
       \newline "}"
       ))

(def emit')
(defmacro emit-sexpr [[& arg]]
  `(generate (~(first arg) ~(map #(emit' %) (rest ~arg)))))

(defmacro emit-sym [arg]
  `(generate arg))

(defmacro emit-sexpr' [[& arg]]
  `(generate (~(first arg) ~@(map (fn [i] `(emit-sym ~i)) (rest arg)))))

(defmacro emit' [arg]
  (if (coll? arg) 
    (emit-sexpr arg)
    (emit-sym arg)))

(def emit)
(defmacro emit "deep walking macro @generating from the bottom up to the root expr" [rexpr]
  (let [[& args] rexpr
        called-symbol (first rexpr)]
    (if (= (count args) 1) ;end expansion here
      `(generate called-symbol)
      ;else: recursively emit dat shit
      `(generate (~(first args) ~(map `emit' (rest args)))) )))
(def call-gen)
(def call-validator)
(defn call [f & args]
  {:type :function
   :symbol 'call
   :genfn #(identity nil) ;;dummy
   :validator #(identity nil) ;;dummy
   :context {}
   :args [f args]}
  )
(defn call-gen [context args])

(def reduce-gen)
(def reduce-validator)

(defn reduce [f init coll]
 {:type :loop
  :symbol 'reduce
  :genfn reduce-gen
  :validator reduce-validator
  :context {}
  :args [f init coll]})

(defn reduce-validator [context args]
  (let [[f init coll] args
        isColl (= :coll (get-type coll))
        collType (inner-type coll)
        initTypeIsCollType (= (get-type init) collType)]
  (and isColl collType initTypeIsCollType)))

(comment
  (generate (subscript (arr 1 2 3) 0))
  (generate (arr 1 2 3)))

(defn reduce-gen [context args]
  (let [[f init coll] args
        elem-type (get-type init)
        loopvar (gensym)
        initvar (gensym)
        n (collsize coll)]
    [[elem-type initvar "=" (generate (f init (subscript coll 0)))]  
     (for-loop loopvar 0 n f)
     ]
    )
  )

(comment ;;example code

(defn square [in]
  (* in in))
;you will need a module statement
(= (get-spirv square)
   "error: argument type of input argument 'in' could not be deduced")
(= (get-spirv (bind-types [in :float] square))
   [
   "%f32 OpTypeFloat 32"
  ]
   )
)
