(ns clsl.core
 (:import  (java.nio ByteBuffer FloatBuffer)
           (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFWErrorCallback GLFWKeyCallback)
           (javax.imageio ImageIO))
 (:require clojure.pprint)
 (:require [clojure.java.io :as io])
 (:gen-class)
 )

(use 'clojure.data)

(defmacro kisslog [path expr] 
  (let [w (gensym)]
    `(.start
       (Thread. 
         (fn [] 
           (with-open [~w (clojure.java.io/writer ~path :append true)]
             (binding [*out* ~w] 
               ~expr)))))))

(defn macroexpand-n [n s-expr]
  (if (< n 1) 
    (macroexpand-1 s-expr)
    (macroexpand-n (dec n) (macroexpand-1 s-expr))))
(defn macroexpand-list [n s-expr]
  (map #(macroexpand-n % s-expr) (range n))) 
(defn macroexpand-readable-list [n s-expr]
  (interpose (apply str (repeat 80 "-")) (macroexpand-list n s-expr))) 
(defn all-public-class-methods [clz]
  (set 
    (org.reflections.ReflectionUtils/getAllMethods clz
                                                   (into-array com.google.common.base.Predicate  
                                                              [(org.reflections.ReflectionUtils/withModifier java.lang.reflect.Modifier/PUBLIC)]))))
(defn all-public-static-class-methods [clz]
  (set 
    (org.reflections.ReflectionUtils/getAllMethods 
      clz
      (into-array com.google.common.base.Predicate 
                  [(org.reflections.ReflectionUtils/withModifier 
                     java.lang.reflect.Modifier/PUBLIC)
                   (org.reflections.ReflectionUtils/withModifier 
                     java.lang.reflect.Modifier/STATIC)]))))
(defn all-public-class-fields [clz]
  (set
    (org.reflections.ReflectionUtils/getAllFields clz
                                                  (into-array com.google.common.base.Predicate  
                                                              [(org.reflections.ReflectionUtils/withModifier java.lang.reflect.Modifier/PUBLIC)]))))
(defn all-public-static-class-fields [clz]
  (set
    (org.reflections.ReflectionUtils/getAllFields 
      clz 
      (into-array com.google.common.base.Predicate 
                  [(org.reflections.ReflectionUtils/withModifier 
                     java.lang.reflect.Modifier/PUBLIC) 
                   (org.reflections.ReflectionUtils/withModifier 
                     java.lang.reflect.Modifier/STATIC)]))))
(defmacro def- [sym-name value] 
  `(def ~(with-meta sym-name {:private true})
        ~value))
;there is most certainly a better way to do this, but it was actually pretty fast to write
(defmacro import-static-all 
  "static imports ALL methods and fields of a java class.
  tries to emulate an \"import static CLASSNAME.*\".
  Conflicting bindings are ignored. For example:
  If you already have a var \"foo\" bound, CLASSNAME/foo will not overwrite foo."
  [input-class-symbol]
  (let [clz (eval input-class-symbol)
        mthds (all-public-static-class-methods clz)
        filtered-methods (filter #(not (resolve (symbol (.getName %)))) mthds)
        grouped-mthds (map 
                  (fn [[m-name l]] (group-by #(count (.getParameterTypes %)) l))
                  (group-by #(.getName %) filtered-methods))
        fields (all-public-static-class-fields clz)
        mthd-defs (map (fn [mem] 
                         (let [mem-name (.getName (first (first (vals mem))))
                               full-name (str (.getName clz) "/" mem-name) 
                               arities (map (fn [[num-count [x & dc]]]
                                              (let [params (map (fn [_] (gensym)) (range num-count))
                                                    body (concat (list (symbol full-name)) params) 
                                                    ]
                                              (list (vec params) body))) 
                                            mem)]
                           (list 'fn [] (concat (list 'defn-) [(symbol mem-name)] arities)))) 
                       grouped-mthds
                       )
        field-defs (map (fn [mem] 
                         (let [body (symbol (str (.getName clz) "/" (.getName mem)))  ]
                                 (list 'fn [] (list 'def- (symbol (.getName mem)) body)))) 
                       (filter #(not (resolve (symbol (.getName %)))) fields))
        all-defs (concat mthd-defs field-defs)
        ]
      `(reduce #(%2) nil ~(vec all-defs))))

(defmacro import-static-all-multiple [clzs]
  (let [classes (if (symbol? clzs) (eval clzs) clzs)]
    `(reduce #(%2) nil ~(vec (map #(list 'fn [] (list 'import-static-all %)) classes)))))

(def GL_IMPORTS 
  '(org.lwjgl.opengl.GL11
    org.lwjgl.opengl.GL12
    org.lwjgl.opengl.GL13
    org.lwjgl.opengl.GL14
    org.lwjgl.opengl.GL15
    org.lwjgl.opengl.GL20
    org.lwjgl.opengl.GL21
    org.lwjgl.opengl.GL30
    org.lwjgl.opengl.GL31
    org.lwjgl.opengl.GL32
    org.lwjgl.opengl.GL33
    org.lwjgl.opengl.GL40
    org.lwjgl.opengl.GL41
    org.lwjgl.opengl.GL42
    org.lwjgl.opengl.GL43
    org.lwjgl.opengl.GL44
    org.lwjgl.opengl.GL45))

(import-static-all-multiple GL_IMPORTS)
(import-static-all org.lwjgl.glfw.GLFW)

(defn shader-valid? [shaderdef]
  (or
   (list? shaderdef)
   (vector? shaderdef)))

(defn to-prefix-fn [coll]
  (if (not (sequential? coll))
    coll
    (interpose 
      (first coll) 
      (map to-prefix-fn (rest coll)))))

(defmacro to-prefix [infix]
  (to-prefix-fn `~infix))

(defonce global-state
  (atom  
    {:internals 
      {:errorCallback   nil
       :keyCallback     nil
       :window-handle   nil
       :width           0
       :height          0
       :title           "clsl demo"
       :drawers {}
       }
      }))
(defn reset-global-state! []
  (reset! global-state
    {:internals 
      {:errorCallback   nil
       :keyCallback     nil
       :window-handle   nil
       :width           0
       :height          0
       :title           "clsl demo"
       :drawers {}
       }
      }))

(def byte-array-type (Class/forName "[B"))
(def short-array-type (Class/forName "[S" ))
(def int-array-type (Class/forName "[I" ))
(def float-array-type (Class/forName "[F" ))
(def double-array-type (Class/forName "[D" ))

(def size-type-map 
  {:float 4
   :int 4

   :vec2 (* 2 4)
   :vec3 (* 3 4)
   :vec4 (* 4 4)
   
   :ivec2 (* 2 4)
   :ivec3 (* 3 4)
   :ivec4 (* 4 4)
   
   :mat2 (* 2 2 4)
   :mat2x3 (* 2 3 4)
   :mat2x4 (* 2 4 4)
   :mat4x2 (* 4 2 4)
   :mat3x2 (* 3 2 4)
   :mat3 (* 3 3 4)
   :mat3x4 (* 3 4 4)
   :mat4x3 (* 4 3 4)
   :mat4 (* 4 4 4)
   })

(defn size-of-type [& ts]
  (reduce + (map size-type-map ts)))

;it is somewhat not-fun to make a transpiler using:
;no function-pointers/functors/functions as values, no dynamic features, no polymorphism, and several other little pitfalls 
;We used a direct transpiler that evaluates expressions to syntax tree nodes with as much information from the get-go as possible. 
;These were then mapped against a set of functors, mapping them to alternative implementations, now in the laguage of choice. 
;The main problem that makes such an approach infeasible is scalibility.
;Adding new functionality is easy but it complicates the whole system.
;You cannot test one feature alone because the final mapping might change with each introduced construct.
;The "standard" way of dealing with this kind of problem is usually to make the compiler use multiple layers that each perform a certain task.
;New functions can be expressed as compositions of these symbolic representations and other functions that build upon them
;adding functionality now does not mess up some system that tries to tie all implemented functions together
;This presumes that all "layers" are somewhat indepedent. As you will see below, they are not really.
;Another approach would be letting the user use the symbolic representation of the more low level gpu-commands. Here however you probably can't comprehend the output at all. This is probably a much better solution overall, but I would say only if you have a working solution as a baseline to compare against.

;glsl type system stub:
(defn mul-t [a b]
  (let 
    [base-a (keyword (apply str (rest (reverse (rest (reverse (str a)))))))
     base-b (keyword (apply str (rest (reverse (rest (reverse (str b)))))))
     elems-a (- (int (last (str a))) 48)
     elems-b (- (int (last (str b))) 48)
     a-mat? (= base-a :mat)
     a-vec? (= base-a :vec)
     b-mat? (= base-b :mat)
     b-vec? (= base-b :vec)]
    (cond
      (and a-mat? b-mat? (= elems-a elems-b)) a

      (and a-mat? b-vec? (= elems-a elems-b)) b
      (and b-mat? a-vec? (= elems-a elems-b)) a
 
      (and a-vec? b-vec? (= elems-a elems-b)) a
       
      (and (or a-mat? a-vec?) 
           (or (= b :float)
               (= b :int))) a
          
      (and (or b-mat? b-vec?) 
           (or (= a :float) 
               (= a :int))) b

      (or (= a :float) (= b :float)) :float
      (and (= a :int) (= b :int)) :int

      :auieee (throw 
                (IllegalArgumentException. 
                  (str "TYPE ERROR ON MULTIPLY.
                       UNDEFINED MULTIPLICATION OF TYPES: " 
                       a b))))))

(defn plus-t [a b]
 (let 
   [base-a (keyword (apply str (rest (reverse (rest (reverse (str a)))))))
    base-b (keyword (apply str (rest (reverse (rest (reverse (str b)))))))
    elems-a (- (int (last (str a))) 48)
    elems-b (- (int (last (str b))) 48)
    a-vec? (= base-a :vec)
    b-vec? (= base-b :vec)]
   (cond
     (and a-vec? b-vec? (= elems-a elems-b)) a
    
     (or (= a :float) (= b :float)) :float
     (and (= a :int) (= b :int)) :int

     :auieee (throw 
               (IllegalArgumentException. 
                 (str "TYPE ERROR ON ADDITION. 
                      UNDEFINED ADDITION OF TYPES: " 
                      a b))))))

(defn minus-t [a b]
 (let 
   [base-a (keyword (apply str (rest (reverse (rest (reverse (str a)))))))
    base-b (keyword (apply str (rest (reverse (rest (reverse (str b)))))))
    elems-a (- (int (last (str a))) 48)
    elems-b (- (int (last (str b))) 48)
    a-vec? (= base-a :vec)
    b-vec? (= base-b :vec)]
   (cond
     (and a-vec? b-vec? (= elems-a elems-b)) a
    
     (or (= a :float) (= b :float)) :float
     (and (= a :int) (= b :int)) :int

     :auieee (throw 
               (IllegalArgumentException. 
                 (str "TYPE ERROR ON SUBSTRACTION. 
                      UNDEFINED SUBSTRACTION OF TYPES: " 
                      a b))))))

(defn add-type-fn [& args]
  (reduce plus-t args))

(defn sub-type-fn [& args]
  (reduce minus-t args))

(defn mul-type-fn [& args]
  (reduce mul-t args))

(def pp clojure.pprint/pprint)

(def built-in-fns
  ["add"
   "sub"
   "mul"
   "div"
   "sin"
   "cos"
   ])

(defn typed [typeinf & args] ;pretty much a no-op. just manual type information :/
  {:type :fn
   :name "typed"
   :typeinf typeinf
   :args args})

(defn add [& args]
  {:type :fn
   :name "add"
   :args args})

(defn sub [& args]
  {:type :fn
   :name "sub"
   :args args})

(defn mul [& args]
  {:type :fn
   :name "mul"
   :args args})

(defn pow [n exp]
  {:type :fn
   :name "pow"
   :n n
   :exp exp})

(defn vec2 [& args]
  {:type :fn
   :name "vec2"
   :args args})

(defn vec3 [& args]
  {:type :fn
   :name "vec3"
   :args args})

(defn vec4 [& args]
  {:type :fn
   :name "vec4"
   :args args})

(defn refract
"For a given incident vector I, surface normal N and ratio of indices of refraction, eta, refract returns the refraction vector, R.

R is calculated as:

    k = 1.0 - eta * eta * (1.0 - dot(N, I) * dot(N, I));
    if (k < 0.0)
        R = genType(0.0);       // or genDType(0.0)
    else
        R = eta * I - (eta * dot(N, I) + sqrt(k)) * N;

The input parameters I and N should be normalized in order to achieve the desired result. " 
  [i n eta]
  {:type :fn
   :name "refract"
   :i i
   :n n
   :eta eta})

(defn dot 
  "dot returns the dot product of two vectors, x and y. i.e., x[0]⋅y[0]+x[1]⋅y[1]+..."
  [x y]
  {:type :fn
   :name "dot"
   :x x
   :y y})

(defn max [& args]
  {:type :fn
   :name "max"
   :args args})

(defn normalize [arg]
  {:type :fn
   :name "normalize"
   :arg arg})

(defn reflect 
"For a given incident vector I and surface normal N reflect returns the reflection direction calculated as I - 2.0 * dot(N, I) * N.

N should be normalized in order to achieve the desired result. "
  [i n]
  {:type :fn
   :name "reflect"
   :i i
   :n n})

(defn reduce' [reduce-fn coll]
  {:type :fn
   :name "reduce"
   :reduce-fn reduce-fn
   :coll coll})

(defn cast-to-float [args]
  {:type :fn
   :name "cast-to-float"
   :args args})

(defn cast-to-int [args]
  {:type :fn
   :name "cast-to-int"
   :args args})

(defn cast-to-uint [args]
  {:type :fn
   :name "cast-to-uint"
   :args args})

(defn cast-to-bool [args]
  {:type :fn
   :name "cast-to-bool"
   :args args})

(defn defvar 
  ([var-name var-type]
    {:type :construct
     :name "vardef"
     :var-type var-type
     :var-name var-name
     :primitive? true})
  ([var-name var-type var-qualifier]
    (assoc (defvar var-name var-type) :var-qualifier var-qualifier))
  ([var-name var-type var-qualifier layout]
    (assoc (defvar var-name var-type) 
           :var-qualifier var-qualifier
           :layout layout)))

(defn scope [var-defs inner]
  {:type :construct
   :name "scope"
   :primitive? true
   :var-defs var-defs
   :inner inner})

(defn sizeof [coll]
  {:type :fn
   :name "sizeof"
   :primitive? true
   :collection coll})

(defn read-at [coll index]
  {:type :fn
   :name "read-at"
   :primitive? true
   :operand coll
   :index index})

(defn write-at [coll index value]
  {:type :side-effect
   :name "write-at"
   :primitive? true
   :operand coll
   :index index
   :value value})

(defn assign [variable value]
  {:type :side-effect
   :name "assign"
   :primitive? true
   :operand variable
   :value value})

(defn for-loop [from to index-name inner] ;make sure the index name does not clash with another name (TODO?)
  {:type :construct
   :name "for"
   :primitive? true
   :index (defvar index-name :int)
   :to "TODO"
   :inner inner})

(defn var-reference
  ([var-name var-type]
    {:type :construct
     :name "varref"
     :var-type var-type
     :var-name var-name
     :primitive? true})
  ([var-name var-type var-qualifier]
    (assoc (var-reference var-name var-type) :var-qualifier var-qualifier)))

(defn sample [sampler texcoords]
  {:type :fn
   :name "sample"
   :sampler sampler
   :texcoords texcoords})

(defn swizzle [elem swizzle-str]
  {:type :fn
   :name "swizzle"
   :elem elem
   :swizzle-str swizzle-str})

(def qualifiers
  #{:in :uniform :out})

(def types
  #{:bool
    :float
    :int :uint
    :mat2 :mat3 :mat4
    :vec2 :vec3 :vec4
    :bvec2 :bvec3 :bvec4
    :ivec2 :ivec3 :ivec4
    :uvec2 :uvec3 :uvec4})

(defn name-type-qualifier-single [v]
  (cond
    (qualifiers v) [:qualifier v]
    (types v) [:type v]
    (number? v) [:layout v]
    :else [:name v]))

(defn parse-vardec [args]
  (reduce (fn [acc v] 
            (let [smap (name-type-qualifier-single v)]
              (if (not (contains? (last acc) (first smap)))
                (assoc acc (dec (count acc)) (assoc (last acc) (first smap) (last smap)))
                (conj acc {(first smap) (second smap)})))) 
          [{}] args))

;a meta-macro?
(defmacro shader-variant 
  "meta shader macro. use to implement various shader variants.
  The higher arity versions accept macros, that are evaluated appropriately beforehand and can modify the body or the inputs to the resulting shader"
  ([impl-fn input-bindings body]
    (let [binders (parse-vardec input-bindings)
          varbinds (apply concat (map 
                                   #(list (:name %) 
                                          (update % :name 
                                                  (fn [sym] `(quote ~sym)))) 
                                   binders))
          inner-binders (map #(var-reference (:name %) (:type %) (:qualifier %)) binders) 
          inner-varbinds (apply concat (map 
                                         #(list (:var-name %) 
                                                (update % :var-name 
                                                        (fn [sym] `(quote ~sym)))) 
                                         inner-binders))]
      `(let [~@inner-varbinds]
         (~impl-fn '~varbinds ~body))))
  ([impl-fn input-bindings body body-mod-macro]
    `(shader-variant ~impl-fn ~input-bindings 
                     (~body-mod-macro ~input-bindings ~body )))
  ([impl-fn input-bindings input-mod-macro body body-mod-macro]
    `(shader-variant ~impl-fn 
                     (~input-mod-macro ~input-bindings ~body) 
                     (~body-mod-macro ~input-bindings ~body)))
  )


(defn massoc [m kv-pairs]
  (apply (partial assoc m) kv-pairs))
(defn assoc-all-as [m1 coll v]
  (massoc m1 (flatten 
               (map vector 
                    coll
                    (repeat (count coll) v)))))

(defn shaderfn* [vars body]
  {:type :shaderfn
   :vardecs vars
   :bound-vars []
   :body body})
(defmacro shaderfn [input-bindings body]
  `(shader-variant shaderfn* ~input-bindings ~body))

(defmacro vertex-shader-output [input-bindings body ]
  (cond 
    (map? body) ;yes: assign each output (the user knows whats up)
    (vec (map (fn [[k v]] `(assign ~k ~v)) body))

    (vector? body) ;functional style. just have a few ouputs. generate assign names
    (vec 
      (concat 
        [`(assign "gl_Position" ~(first body))]
        (map-indexed
          (fn [index entry] `(assign ~(str "vert_shdr_out_" index) ~entry) ) 
          (rest body))))

    :default ;implicit gl_pos assign
    `(assign :f3d/pos ~body)))
(defmacro shader-output-input-binding-mod [input-bindings body]
  (if (map? body)
    ;yes: assign each output (the user knows whats up)
    `(vec (concat '~input-bindings (interpose :out (keys '~body))))
    ;implicit (gl_pos, gl_color) assign. no further output
    '~input-bindings))

(defn shader-output-input-binding-modfn [input-bindings body]
  (cond (map? body) ;yes: assign each output (the user knows whats up)
    (vec (concat 
           input-bindings 
           (apply 
             concat 
             (map vector (filter symbol? (keys body)) 
                  (repeat (count (filter symbol? (keys body))) :out)))))
    ;(vector? body) ; first is gl_position. but add rest to input binds
    ;(concat input-bindings ['lulululu "bittewas?"])
    :default ;implicit (gl_pos, gl_color) assign. no further output
    input-bindings))
(defn vertex-shader-output-input-binding-modfn [input-bindings body]
  (cond (map? body) ;yes: assign each output (the user knows whats up)
    (vec (concat 
           input-bindings 
           (apply 
             concat 
             (map vector (filter symbol? (keys body)) 
                  (repeat (count (filter symbol? (keys body))) :out)))))
    (vector? body) ; first is gl_position. but add rest to input binds
    (vec 
      (concat input-bindings 
              (apply concat 
                     (map-indexed 
                       (fn [index entry] 
                         [(symbol (str "vert_shdr_out_" index)) :out index]) 
                       (rest body)))))
    :default ;implicit (gl_pos, gl_color) assign. no further output
    input-bindings))
(defn fragment-shader-output-input-binding-modfn [input-bindings body]
  (if (map? body)
    ;yes: assign each output (the user knows whats up)
    (vec (concat 
           input-bindings 
           (apply 
             concat 
             (map vector (filter symbol? (keys body)) 
                  (repeat (count (filter symbol? (keys body))) :out)))))
    ;implicit (gl_pos, gl_color) assign. no further output
    (vec (concat input-bindings ['outColor :vec4 :out]))))
(defn vertex-shader* [vars body]
  {:type :shader
   :name "vertex-shader"
   :vardecs vars
   :bound-vars []
   :primitive? false
   :body body}) ;TODO: implement functionality?
(defmacro vertex-shader [input-bindings body]
  (let [modin (vertex-shader-output-input-binding-modfn
                input-bindings body)] 
    `(shader-variant vertex-shader* 
                   ~modin
                   ~body vertex-shader-output)))

(defmacro fragment-shader-output [input-bindings body ]
  ;implicit outcolor assign
  `(assign (var-reference "outColor" :vec4 :out) ~body))

(defn fragment-shader* [vars body]
  {:type :shader
   :name "fragment-shader"
   :vardecs vars
   :bound-vars []
   :primitive? false
   :body body}) ;TODO: implement functionality?

(defmacro fragment-shader [input-bindings body]
  (let [modin (fragment-shader-output-input-binding-modfn
                input-bindings body)] 
    `(shader-variant fragment-shader* ~modin 
                   ~body fragment-shader-output)))

(defn vardec-to-vardef [vardec]
  (map (fn [decl]
    (apply defvar (map decl [:name :type :qualifier :layout])))
       vardec))
(defn argument-by-name [sname args]
  (first (filter #(= sname (:name %)) args)))

(defn shader-output [shadr]
  (sort-by :layout (filter #(= :out (:qualifier %)) (filter map? (:vardecs shadr)))))

(defn simple-pipeline* [primed-v-shader 
                        primed-f-shader 
                        interface]
  {:name "pipeline"
   :vertex-shader primed-v-shader
   :fragment-shader primed-f-shader
   :pipeline-interface interface})
(defmacro simple-pipeline 
  "new pipeline version. Use to create a simplified version of pipelines.
  First specify the inputs to the pipeline (just like a function def). Then a vector of the primed shaders that make up the pipeline.
  Support for tesselation(control/eval) has been explicitly removed. Use compute shaders instead.
  Simplest version only has vertex and fragment shader stages in body."
  [interface body] 
;TODO: maybe not use just keyword of the symbols? possible clash
;amend: this keyword behaviour has been so long the default behaviour, "fixing" this bug will cause breakage!
;TODO: simply add to documentation
  `(let ~(vec 
           (concat 
             (interleave interface (map keyword interface))
             ['evaluated-body body]))
     (simple-pipeline* 
       (first ~'evaluated-body) 
       (second ~'evaluated-body)
       (map keyword ~interface))))
(defn if-not-map-then-uniform [in]
  (if (map? in)
    in
    {:name in :qualifier :uniform}))

;Uniform layout locations are written interleaved!
;vertex shader uniform locations are even, fragment shader uniform locations odd!
(defn prime-shader [shdr & input-vars]
  (cond 
    (= "vertex-shader" (:name shdr))
      (assoc shdr :bound-vars input-vars)
    (= "fragment-shader" (:name shdr))
      (let [uniforms 
            (filter (fn [[_ item]] (not (map? item))) 
                    (map-indexed (fn [index item] [index item]) input-vars))
            with-bound-vars (assoc shdr :bound-vars input-vars)]
        (reduce (fn [acc [ind uni]]
                  (update-in 
                    (update acc :vardecs vec) 
                             [:vardecs (inc (* 2 ind))] ; because im a big dum dum
                             (fn [varmap]
                               (assoc
                                 (assoc varmap :qualifier :uniform)
                                  :layout (inc (* ind 2))))))
                with-bound-vars
                uniforms))))

(defn glsl-keyword-type-to-num-and-elem-type [t]
  (cond (= :float t) [1 GL_FLOAT]
        (= :vec2 t)  [2 GL_FLOAT]
        (= :vec3 t)  [3 GL_FLOAT]
        (= :vec4 t)  [4 GL_FLOAT]
        (= :mat2 t)  [4 GL_FLOAT]
        (= :mat3 t)  [9 GL_FLOAT]
        (= :mat4 t)  [16 GL_FLOAT]
        
        (= :int t)    [1 GL_INT]
        (= :ivec2 t)  [2 GL_INT]
        (= :ivec3 t)  [3 GL_INT]
        (= :ivec4 t)  [4 GL_INT]))

;clsl to glsl input type matching:
;glm support isn't pretty, but it works.
;maybe with proper Clojure support this can just vanish
(defprotocol clojure-t->glsl-t
  "converts clojure values/java objects to matching glsl-types
If you don't like the defaults 
(use a different matrix lib, treat arraylists as vec_n, etc...)
just extend the protocol to your liking"
  (convert-type [obj])
  (load-value-to-array [obj]))
(extend-protocol clojure-t->glsl-t
  clojure.lang.Keyword
    (convert-type [o] o)
  ;no-load-value!
  clojure.lang.IPersistentMap
    (convert-type [o] (:glsl-type o))
    ;those are indirect values supplied by buffers or textures
    ;loading only makes sense for textures here. buffers are explicitly bound
    (load-value-to-array [o] 
      (cond 
        (= :texture (:glsl-type o)) (:tex-id o)
        (= :buffer-texture (:name o)) [(:tex-id o) (:buf-id o)])) 
    ;load-value-to-array doesn't actually return an array. 
    ;Maybe rethink this protocols name?
  Long 
    (convert-type [o] :int)
    (load-value-to-array [o] (let [arr (int-array 1)] (aset arr 0 o) arr))
  Float 
    (convert-type [o] :float)
    (load-value-to-array [o] (let [arr (float-array 1)] (aset arr 0 o) arr))
   Double
    (convert-type [o] :float)
    (load-value-to-array [o] (let [arr (float-array 1)] (aset arr 0 (float o)) arr))
  clojure.lang.PersistentVector 
    (convert-type [o] (keyword (str "vec" (count o))))
    (load-value-to-array [o]  (amap ^floats (float-array (count o)) idx ret (float (nth o idx))))
  ;matrix types: use glm types as default.
  glm.mat2x2.Mat2 
    (convert-type [o] :mat2)
    (load-value-to-array [o] (let [arr (float-array 4)] 
                               (aset arr 0 (.v00 o)) (aset arr 1 (.v01 o)) 
                               (aset arr 2 (.v10 o)) (aset arr 3 (.v11 o)) 
                               arr))
  glm.mat3x3.Mat3 
    (convert-type [o] :mat3)
    (load-value-to-array [o] (let [arr (float-array 9)] 
                               (aset arr 0 (.v00 o)) (aset arr 1 (.v01 o)) (aset arr 2 (.v02 o))
                               (aset arr 3 (.v10 o)) (aset arr 4 (.v11 o)) (aset arr 5 (.v12 o))
                               (aset arr 6 (.v20 o)) (aset arr 7 (.v21 o)) (aset arr 8 (.v22 o))
                               arr))
  glm.mat4x4.Mat4 
    (convert-type [o] :mat4)
    (load-value-to-array [o] (let [arr (float-array 16)] 
     (aset arr 0 (.v00 o)) (aset arr 1 (.v01 o)) (aset arr 2 (.v02 o)) (aset arr 3 (.v03 o))
     (aset arr 4 (.v10 o)) (aset arr 5 (.v11 o)) (aset arr 6 (.v12 o)) (aset arr 7 (.v13 o))
     (aset arr 8 (.v20 o)) (aset arr 9 (.v21 o)) (aset arr 10 (.v22 o)) (aset arr 11 (.v23 o))
     (aset arr 12 (.v30 o)) (aset arr 13 (.v31 o)) (aset arr 14 (.v32 o)) (aset arr 15 (.v33 o))
       arr))
  )
;OH GOD. i need seperate files for all this crap
(defn interface-type-map [drawer init-state] 
  (let [interface-fill-fn (:interface-fill drawer)
        interface-fill ((:interface-fill drawer) init-state)] 
    (map convert-type interface-fill)))

(defn interface-modifier-map [drawer init-state] 
  (map #(if (and (map? %) (= :buf-take (:name %))) :in :uniform) ((:interface-fill drawer) init-state))) 

(defmacro with-glGetError [expr] 
 `(let [~'res ~expr
        ~'err (glGetError)]
    (if (= ~'err 0) nil (do (println (str "err after " '~expr ": " ~'err))
                            (throw (NullPointerException. "AHHH"))))
    ~'res))
;language primitives. what a parser would usually understand in an interpreter. They are ready to be emitted/interpreted.
;Some higher-level concepts are not emitable because they lack the necessary information
;For example the shaders do not need to be immediately emitable since the input can be bound later when combind to a shader pipeline.

(defmulti to-primitives 
  #(if (sequential? %) :sequential (% :name)))

(defmethod to-primitives :default [params]
  ;no conversion method registered. assuming already emitable
  params)

(defmethod to-primitives :sequential [params]
  (vec (map to-primitives params)))

(defmethod to-primitives "reduce" [arg]
  ((:reduce-fn arg) (:coll arg)))

(defmethod to-primitives "reduce" [arg]
  (let [index (str "index_" (gensym))
        res (str "res_" (gensym))] 
    [(defvar res :replace-me-type)
     (for-loop 0 (sizeof (:coll arg)) index 
               (assign res ((:reduce-fn arg) res (read-at (:coll arg) index))))]))

(defmethod to-primitives "sample" [arg]
  (update arg :texcoords to-primitives))

(defmethod to-primitives "add" [arg]
  (update arg :args to-primitives))

(defmethod to-primitives "mul" [arg]
  (update arg :args to-primitives))

(defmethod to-primitives "sub" [arg]
  (update arg :args to-primitives))

(defmethod to-primitives "div" [arg]
  (update arg :args to-primitives))

(defmethod to-primitives "swizzle" [arg]
  (update arg :elem to-primitives))

(defmethod to-primitives "refract" [arg]
  (assoc arg
         :i (to-primitives (:i arg))
         :n (to-primitives (:n arg))
         :eta (to-primitives (:eta arg))))

(defmethod to-primitives "dot" [arg]
  (assoc arg
         :x (to-primitives (:x arg))
         :y (to-primitives (:y arg))))

(defmethod to-primitives "max" [arg]
  (update arg :args to-primitives))

(defmethod to-primitives "normalize" [arg]
  (update arg :arg to-primitives))

(defmethod to-primitives "reflect" [arg]
  (assoc arg
         :i (to-primitives (:i arg))
         :n (to-primitives (:n arg))))

(defmethod to-primitives "vertex-shader" [arg]
  (assoc arg
         :vardecs (vec
                    (vardec-to-vardef 
                      (flatten (partition 1 2 (rest (:vardecs arg))))))
         :body (to-primitives (:body arg))
         :primitive? true))

(defmethod to-primitives "fragment-shader" [arg]
  (assoc arg
         :vardecs (vec
                    (vardec-to-vardef 
                      (flatten (partition 1 2 (rest (:vardecs arg))))))
         :body (to-primitives (:body arg))
         :primitive? true))

(defn in-out-shader-transition [shader1 shader2]
  {:s1-out (filter 
             #(if (map? %) (= :out (:qualifier %)) nil) 
             (:vardecs shader1))
   :s2-in (filter #(if (map? %) (= :out (:qualifier %)) nil) (:vardecs shader2))})

(defn assoc-truthy-only [m & kvs] 
  (reduce 
    (fn [acc [k v]] 
      (if v 
        (assoc acc k v) 
        acc))
    m
    (partition 2 2 kvs)))

(defn in-vardecs [vardecs]
  (filter (fn [v] (if (map? v) (= :in (:qualifier v)) false)) vardecs))

(defn out-vardecs [vardecs]
  (filter (fn [v] (if (map? v) (= :out (:qualifier v)) false)) vardecs))

(def glsl-type-to-image-format ; a one to one mapping. no fn needed.
  {:float     GL_R32F
   ;:byte      GL_R8I
   ;:short     GL_R16I 	
   :int       GL_R32I 	
   ;:ubyte     GL_R8UI 	
   ;:ushort    GL_R16UI
   :uint      GL_R32UI
   
   :vec2      GL_RG32F
   :ivec2     GL_RG32I
   :uvec2     GL_RG32UI
   
   :vec3      GL_RGB32F
   :ivec3     GL_RGB32I
   :uvec3     GL_RGB32UI
   
   :vec4      GL_RGBA32F
   :ivec4     GL_RGBA32I
   :uvec4     GL_RGBA32UI}) 

(def samplerBuffer-type-to-image-format
  {"samplerBuffer"     (glsl-type-to-image-format :float)
   "isamplerBuffer"    (glsl-type-to-image-format :int)
   "usamplerBuffer"    (glsl-type-to-image-format :uint)

   "samplerBuffer "    (glsl-type-to-image-format :vec2)
   "isamplerBuffer "   (glsl-type-to-image-format :ivec2)
   "usamplerBuffer "   (glsl-type-to-image-format :uvec2)

   "samplerBuffer  "   (glsl-type-to-image-format :vec3)
   "isamplerBuffer  "  (glsl-type-to-image-format :ivec3)
   "usamplerBuffer  "  (glsl-type-to-image-format :uvec3)

   "samplerBuffer   "  (glsl-type-to-image-format :vec4)
   "isamplerBuffer   " (glsl-type-to-image-format :ivec4) 
   "usamplerBuffer   " (glsl-type-to-image-format :uvec4)})

(defn uniform-fn-for-glsl-type [t]
  (cond
    (= :int t)   glUniform1iv
    (= :float t) glUniform1fv
    (= :vec2 t)  glUniform2fv
    (= :vec3 t)  glUniform3fv
    (= :vec4 t)  glUniform4fv

    (= :mat2 t)   (fn [loc v] (glUniformMatrix2fv loc false v))
    (= :mat2x3 t) (fn [loc v] (glUniformMatrix2x3fv loc false v))
    (= :mat3x2 t) (fn [loc v] (glUniformMatrix3x2fv loc false v))
    (= :mat4x2 t) (fn [loc v] (glUniformMatrix4x2fv loc false v))

    (= :mat3 t)   (fn [loc v] (glUniformMatrix3fv loc false v))
    (= :mat3x4 t) (fn [loc v] (glUniformMatrix3x4fv loc false v))
    (= :mat4x3 t) (fn [loc v] (glUniformMatrix4x3fv loc false v))

    (= :mat4 t)   (fn [loc v] (glUniformMatrix4fv loc false v))
    
    (= :sampler2D t) (fn [loc v] (glActiveTexture (+ GL_TEXTURE0 loc)) 
                                 (glBindTexture GL_TEXTURE_2D v) 
                                 (glUniform1i loc loc))
    (and (= (type t) java.lang.String) 
         (.contains t "samplerBuffer")) 
           (fn [loc [tex-id buf-id]] 
             (glActiveTexture (+ GL_TEXTURE0 loc)) 
             (glBindTexture GL_TEXTURE_BUFFER tex-id) 
             (glTexBuffer 
               GL_TEXTURE_BUFFER (samplerBuffer-type-to-image-format t) buf-id)
             (glUniform1i loc loc))
    :default (throw (IllegalArgumentException. 
                      (str "cannot create uniform-fn for glsl-type:\n" 
                           (with-out-str (pp t)))))))

;since "primitives" are always emittable, we need to check the global state at this point
;so we can get an init state to have the drawer infer its arguments from.
;or we rely on whoever calls this to first add an init-state. 
;Which has to be @global-state. So for now im keeping the side-effect
(defmethod to-primitives "drawer" [arg]
 ;TODO: REFACTOR! this has to be my most complected clojure code to date
 ;this is a mess. type transfer happens here. Most things happen here.
 ;which means: probably a lot of bugs happen here.
(if (:primitive? arg) arg ;if already emittable, this fn is the identity
(let [
init-state (if (:custom-state arg) (:custom-state arg) @global-state)
i-face-types (interface-type-map arg init-state)
i-face-modifiers (interface-modifier-map arg init-state)
pipe-interface (-> arg :pipe :pipeline-interface)
v-shader-vardecs (-> arg :pipe :vertex-shader :vardecs)
v-shader-bound-vars (-> arg :pipe :vertex-shader :bound-vars)

v-shader-body (-> arg :pipe :vertex-shader :body);
v-shader-manual-out-type-info (map (fn [elem] 
                                     [(:operand elem) 
                                      (-> elem :value :typeinf)]) 
                                   (filter
                                     (fn [elem] (= "typed" (-> elem :value :name)))
                                     v-shader-body))

f-shader-vardecs (-> arg :pipe :fragment-shader :vardecs)
f-shader-bound-vars (-> arg :pipe :fragment-shader :bound-vars)
inputs (partition 3 3 (interleave pipe-interface i-face-types i-face-modifiers))

vert-outs (map (fn [vardec] 
                 (assoc vardec 
                        :type (second 
                                (first 
                                  (filter 
                                    #(= (first %) (str (eval (:name vardec)))) 
                                    v-shader-manual-out-type-info)))
                        :layout (inc (* 2 (first (first (filter #(= (:name vardec) (-> % second :name)) (filter (comp map? second) (map-indexed vector f-shader-bound-vars)))))))
                        )) 
               (out-vardecs v-shader-vardecs))

parted-v-shader-vardecs (partition 2 2 v-shader-vardecs)
parted-f-shader-vardecs (partition 2 2 f-shader-vardecs)
vert-nouts (filter 
             (fn [[sym decl]] 
               (not 
                 (reduce #(and %1 %2)
                         (map #(= (:name decl) (:name %)) vert-outs))))
             parted-v-shader-vardecs)

frag-outs (out-vardecs f-shader-vardecs)

frag-nouts-raw (filter 
             (fn [[sym decl]] 
               (not 
                 (reduce #(and %1 %2)
                         (map #(= (:name decl) (:name %)) frag-outs))))
             parted-f-shader-vardecs)
frag-nouts (map (fn [[sym decl]]
              [sym 
               (assoc decl
                     :qualifier (if (not (empty? (filter #(= sym (eval (:name %))) vert-outs))) 
                                  :in
                                  :uniform))]) 
              frag-nouts-raw)
f-shader-bound-vars-layout-map (zipmap f-shader-bound-vars (map #(inc (* 2 %)) (range)))
new-v-shader-nout-vardecs (map-indexed (fn [index varname]
                          (let [[kwname kwtype kwmod] 
                                (apply concat (filter #(= varname (first %)) 
                                  inputs))
                                [sym vardecl] (nth parted-v-shader-vardecs index)]
                            (list sym 
                                  (assoc-truthy-only
                                    vardecl
                                    :type kwtype 
                                    :qualifier kwmod
                                    :layout (* 2 index))))
                          ) v-shader-bound-vars)

new-v-shader-out-vardecs (map #(list (eval (:name %)) %) vert-outs)

new-v-shader-vardecs (concat 
                       new-v-shader-nout-vardecs
                       new-v-shader-out-vardecs)

new-f-shader-nout-vardecs (map-indexed (fn [index varname]
                          (let [[kwname kwtype kwmod] 
                                (apply concat (filter #(= varname (first %)) 
                                  inputs))
                                [sym vardecl] (nth parted-f-shader-vardecs index)
                                layout (inc (* 2 index))
                                corr-inf (first (filter #(= layout (:layout %)) vert-outs))]
                            (list sym 
                                  (assoc-truthy-only
                                    vardecl
                                    :type (if corr-inf (:type corr-inf) kwtype) 
                                    :qualifier (if 
                                                 corr-inf ;not (empty? (filter #(= sym (eval (:name %))) vert-outs))
                                                 :in 
                                                 :uniform)
                                    ;:qualifier kwmod
                                    :layout layout)))
                          ) f-shader-bound-vars)
new-f-shader-vardecs (concat
                       new-f-shader-nout-vardecs
                       (map #(list (eval (:name %)) %) frag-outs))
v-shader-uniforms (filter (fn [elem] (= :uniform (:qualifier elem)))
                          (map second new-v-shader-nout-vardecs))
f-shader-uniforms (filter (fn [elem] (= :uniform (:qualifier elem))) 
                          (map second new-f-shader-nout-vardecs))
all-uniform-variables (concat v-shader-uniforms f-shader-uniforms)

uniform-var-tuples 
(group-by 
        first 
        (map (fn [[var-name layout]] [var-name layout])
             (concat 
               (partition 2 2 (interleave v-shader-bound-vars (map #(* 2 %) (range))))
               (partition 2 2 (interleave f-shader-bound-vars (map #(inc (* 2 %)) (range)))))))
uniform-layout-to-interface-index-list 
(apply concat (map (fn [[v-name ind]] 
           (map #(vector % ind) (map second (uniform-var-tuples v-name)))) 
         (map #(vector (first (first %)) (second %)) (filter #(= :uniform (second (rest (first %)))) (partition 2 2 (interleave inputs (range)))))))
pipe-in-variables (filter (fn [elem] 
                            (= :in (:qualifier elem))) 
                          (map second new-v-shader-nout-vardecs))

interface-init-fill ((:interface-fill arg) init-state)

init-fn (fn [drawer]
             (let[
               vao-id (glGenVertexArrays)
               _ (glBindVertexArray vao-id)
               i-in-fill (map (comp (partial nth interface-init-fill) second) 
                              (filter (comp (partial = :in) first) 
                                      (partition 2 2 (interleave i-face-modifiers (range)))))
               _ (dotimes [i (count (filter #(= :in %) i-face-modifiers))]
                   (let [i-var (nth pipe-in-variables i)]
                     (glBindBuffer 
                       GL_ARRAY_BUFFER 
                       (:buf-id (nth i-in-fill i)))
                     (glEnableVertexAttribArray (:layout i-var))
                     (glVertexAttribPointer 
                       ;layout location
                       (:layout i-var)
                       ;num-elements
                       (first (glsl-keyword-type-to-num-and-elem-type (:type i-var)))
                       ;GL_TYPE
                       (second (glsl-keyword-type-to-num-and-elem-type 
                         (:type i-var)))
                       ;normalized? (no support for this)
                       false 
                       ;stride
                       (:stride (nth i-in-fill i))
                       ;extra offset to buf
                       (:offset (nth i-in-fill i)))))]
               (assoc drawer :vao vao-id)))
exec-fn (let 
          [uniform-calls (for [[layout interface-index] 
                               uniform-layout-to-interface-index-list] 
                           (let [i-var (first 
                                         (filter #(= (:layout %) layout) 
                                                 all-uniform-variables))] 
                             (partial 
                               (uniform-fn-for-glsl-type (:type i-var))
                               layout)))
           index-list (map second uniform-layout-to-interface-index-list)]
            (fn [drawer latest-state]
              (let [curr-interface-fill ((:interface-fill arg) latest-state)]
                (map-indexed #(list %2 (load-value-to-array 
                                      (nth curr-interface-fill (nth index-list %1)))) 
                               uniform-calls)

                (glUseProgram (:program drawer))
                (glBindVertexArray (:vao drawer))
                (doall 
                  (map-indexed #(%2 (load-value-to-array 
                                      (nth curr-interface-fill (nth index-list %1)))) 
                               uniform-calls))
                ((:drawcmds-fn arg) latest-state))))

new-pipe (assoc-in 
           (assoc-in (:pipe arg) 
                      [:vertex-shader :vardecs] (apply concat new-v-shader-vardecs))
                      [:fragment-shader :vardecs] (apply concat new-f-shader-vardecs))

]
;procedure:
;compile -> init-fn -> loop exec-fn
(assoc arg
  :primitive? true
  :pipe (to-primitives new-pipe)
  :interface-types i-face-types
  :interface-modifiers i-face-modifiers
  :interface-init-fill interface-init-fill
  :init-fn init-fn
  :exec-fn exec-fn
  :init-state init-state))))

(defmethod to-primitives "pipeline" [arg]
  (let []
    (assoc arg
          :primitive? true
          :vertex-shader (to-primitives (:vertex-shader arg))
          :fragment-shader (to-primitives (:fragment-shader arg)))))

(defmethod to-primitives "cast-to-float" [arg]
  (assoc arg
        :primitive? true
        :args (to-primitives (:args arg))))

(defmethod to-primitives "cast-to-int" [arg]
  (assoc arg
        :primitive? true
        :args (to-primitives (:args arg))))

(defmethod to-primitives "cast-to-uint" [arg]
  (assoc arg
        :primitive? true
        :args (to-primitives (:args arg))))

(defmethod to-primitives "cast-to-bool" [arg]
  (assoc arg
        :primitive? true
        :args (to-primitives (:args arg))))

(defmethod to-primitives "typed" [arg]
  (assoc arg
         :primitive true
         :args (to-primitives (:args arg))))

(defn primitive-optimize [ast]
  ast;TODO: stub. identity for now. do some optimizsation here
  )

(defn infer-types [ast]
  ast;obsolete?
  )

(defmulti type-infer #(if (vector? %) :vector (% :name)))

(defmethod type-infer :default [params]
  ;no conversion method registered. assuming already emitable
  params)

(defmulti emit-form #(% :name))

(defn emit [arg] 
  (cond 
    (sequential? arg) (reduce #(str %1 "\n" %2) "" (map emit arg))
    (map? arg) (emit-form arg)
    :default arg
    ))

(defmethod emit-form :default [arg]
  arg)

(defmethod emit-form "for" [arg]
  (let [varname (:var-name (:index arg))] 
    (str "for(int " varname "=0;"
         varname "<" (:to arg) ";){\n"
         (emit (:inner arg))
         "\n}"
         )))

(defn name-or-printstr [arg]
  (try (name arg)
       (catch Exception e (print-str arg))))

(defmethod emit-form "vardef" [arg]
  (if (:var-qualifier arg)
    (if (:layout arg) 
      (str "layout( location = " (:layout arg) " ) "
           (name-or-printstr (:var-qualifier arg)) " " 
           (name-or-printstr (:var-type arg)) " " 
           (eval (:var-name arg)) ";") 
      (str (name-or-printstr (:var-qualifier arg)) " " 
           (name-or-printstr (:var-type arg)) " " 
           (eval (:var-name arg)) ";"))
    (str (name-or-printstr (:var-type arg)) " " 
         (eval (:var-name arg)) ";")))

(defmethod emit-form "assign" [arg]
  (str (emit (:operand arg)) "=" (emit (:value arg))))

(defmethod emit-form "read-at" [arg]
  (str (:operand arg) "[" (:index arg) "]"))

(defmethod emit-form "sizeof" [arg]
  (if (symbol? arg)
    (str "sizeof(" arg ")")
    (str (count (:collection arg)))))

(defmethod emit-form "add" [arg]
  (clojure.string/join " + " (map #(str "(" (emit %) ")") (:args arg))))

(defmethod emit-form "sub" [arg]
  (clojure.string/join " - " (map #(str "(" (emit %) ")") (:args arg))))

(defmethod emit-form "mul" [arg]
  (clojure.string/join " * " (map #(str "(" (emit %) ")") (:args arg))))

(defmethod emit-form "vec2" [arg]
  (str "vec2(" 
       (clojure.string/join 
         "," (map #(str "(" (emit %) ")") (:args arg)))
       ")"))

(defmethod emit-form "vec3" [arg]
  (str "vec3(" 
       (clojure.string/join 
         "," (map #(str "(" (emit %) ")") (:args arg)))
       ")"))

(defmethod emit-form "vec4" [arg]
  (str "vec4(" 
       (clojure.string/join 
         "," (map #(str "(" (emit %) ")") (:args arg)))
       ")"))

(defmethod emit-form "swizzle" [arg]
  (str "(" (emit (:elem arg)) ")." (name (:swizzle-str arg))))

(defmethod emit-form "refract" [arg]
  (str "refract(" 
       (emit (:i arg)) "," 
       (emit (:n arg)) ","
       (emit (:eta arg))")"))

(defmethod emit-form "dot" [arg]
  (str "dot(" 
       (emit (:x arg)) "," 
       (emit (:y arg)) ")"))
(defn- create-max-tuples [coll]
  (let [res (map 
              (fn [[a b]] 
                (str "max(" (emit a) "," (emit b) ")")) 
              (partition 2 2 coll))] 
    (if (= 0 (mod (count coll) 2)) 
      res 
      (conj res (last coll)))))
(defn- max-emit [coll] 
  (if (= (count coll) 1)
    coll
    (recur (create-max-tuples coll))))

(defmethod emit-form "max" [arg]
  (emit (max-emit (:args arg))))

(defmethod emit-form "normalize" [arg]
  (str "normalize(" (emit (:arg arg)) ")"))

(defmethod emit-form "reflect" [arg]
  (str "reflect(" 
       (emit (:i arg)) "," 
       (emit (:n arg)) ")"))

(defmethod emit-form "pow" [arg]
  (str "pow(" 
       (emit (:n arg)) "," 
       (emit (:exp arg)) ")"))

(defmethod emit-form "sample" [arg]
  (str "texture(" (emit (:sampler arg)) ", " (emit (:texcoords arg)) ")" ))

(defmethod emit-form "div" [arg]
  (clojure.string/join " / " (map #(str "(" (emit %) ")") (:args arg))))

(defmethod emit-form "cast-to-float" [arg]
  (str "((float) " (emit (:args arg)) ")"))

(defmethod emit-form "cast-to-int" [arg]
  (str "((int) " (emit (:args arg)) ")"))

(defmethod emit-form "cast-to-uint" [arg]
  (str "((uint) " (emit (:args arg)) ")"))

(defmethod emit-form "cast-to-bool" [arg]
  (str "((bool) " (emit (:args arg)) ")"))

(defmethod emit-form "varref" [arg]
  (str (:var-name arg)))

(defmethod emit-form "typed" [arg]
  (emit (:args arg)))

(defmethod emit-form "vertex-shader" [arg]
  (let [body (:body arg)] 
    (str 
      "#version 450\n"
      ;input and uniform vardecs
      (emit (:vardecs arg))
      ;main
      "\nvoid main(){\n"
      ;body
      (if (sequential? body)
        (emit (interleave body (repeat ";")))
        (emit (interleave [body] (repeat ";"))))
      ;main end 
      "\n}"
      )))

(defmethod emit-form "fragment-shader" [arg]
  (let [body (:body arg)] 
    (str 
      "#version 450\n"
      ;input and uniform vardecs
      (emit (:vardecs arg))
      ;main
      "\nvoid main(){\n"
      ;body
      (if (sequential? body)
        (emit (interleave body (repeat ";")))
        (emit (interleave [body] (repeat ";"))))
      ;main end 
      "\n}"
      )))

(defmethod emit-form "pipeline" [arg]
  {:vertex-shader (emit (:vertex-shader arg))
   :fragment-shader (emit (:fragment-shader arg))})

(defmethod emit-form "drawer" [arg]
  {:vertex-shader (emit (:vertex-shader (:pipe arg)))
   :fragment-shader (emit (:fragment-shader (:pipe arg)))})

(defn shader-output-prettify [txt]
  (let [lines (clojure.string/split txt #"\n")] 
    (str
      (first lines) "\n"
      (reduce str 
        (map 
          (fn [[prev cur next]]
            (cond
              (or (= \{ (last cur)) (= \} (first next))) 
                (str cur "\n")
              (or (not (= \; (last cur))) (= next ";")) 
                (str cur)
              :default 
                (str cur "\n")
              ))  
          (partition 3 1 lines)))
      (last lines))))

(defn prettify-compile-out [arg]
  (assoc arg 
         :vertex-shader (shader-output-prettify (:vertex-shader arg))
         :fragment-shader (shader-output-prettify (:fragment-shader arg))))

(defn compile-glsl [arg]
  (-> arg
      (to-primitives)
      (primitive-optimize)
      (infer-types)
      (emit)
      (prettify-compile-out)))

(defn retrieve-nested [nested ks]
  (if ks (recur (nested (first ks)) (next ks)) nested))

;matrix creation. use glm as standard
(comment 
  ;introduce some macro like this?:
 (defmacro glm [f & args] `(~(symbol (str "." f)) glm.glm/INSTANCE ~@args))
  ;or helper fns like these?:
(defn vec2 [] (glm.vec2.Vec2.))
(defn vec3 [] (glm.vec3.Vec3.))
(defn vec4 [] (glm.vec4.Vec4.))

(defn mat2 [] (glm.mat2x2.Mat2.))
(defn mat3 [] (glm.mat3x3.Mat3.))
(defn mat4 [] (glm.mat4x4.Mat4.))
;better yet: get involved with kotlin graphics glm and make clojure support happen
)

;drawers are values, representing one draw process. drawers are decoupled by nature.
;drawers need to specify a query vector of the state they want to access.
;drawers can be registred as sub-drawers creating a hierarchy, introducing implicit dependencies. (vk subpasses) (not implemented-yet)
;a f3d rendering pipeline must be specified to recieve the draw calls
;the interface specified by the pipeline must be satisfied with a vector of inputs. the interface input vector to the pipeline is sadly limited at this time.
;changing the type of the elements after creating the drawer does not work at the time. E.g. you cannot switch between inputting a float and an int as the same parameter.
;doing so might just work, but it might also break spectacularly
;creating a drawer means compiling shaders :(

(defn buf-take 
  ([buf-id glsl-type stride offset]
    {:name :buf-take
     :buf-id buf-id 
     :stride stride 
     :offset offset
     :glsl-type glsl-type})
  ([buf-id glsl-type]
    {:name :buf-take
     :buf-id buf-id 
     :stride 0
     :offset 0
     :glsl-type glsl-type}))

(def element-type-to-samplerBuffer-type
  {:float "samplerBuffer"
   :int   "isamplerBuffer"
   :uint  "usamplerBuffer"

   :vec2  "samplerBuffer "
   :ivec2 "isamplerBuffer "
   :uvec2 "usamplerBuffer "

   :vec3  "samplerBuffer  "
   :ivec3 "isamplerBuffer  "
   :uvec3 "usamplerBuffer  "

   :vec4  "samplerBuffer   "
   :ivec4 "isamplerBuffer   "
   :uvec4 "usamplerBuffer   "})

(defn buf-as-texture
  "create buffer-texture out of a buffer.
   Meaning: use buffer buf-id as a random access array in your shader.
   read them in the shader with the \"texel-fetch\" function
   element-type is how the data from the buffer is to be read out.
   element-type can be any shader type, like :int, :uint, :float, vec2, ivec4, etc.."
  [buf-id element-type]
  {:name :buffer-texture
   :buf-id buf-id 
   :tex-id (glGenTextures)
   :element-type element-type
   :glsl-type (element-type-to-samplerBuffer-type element-type)})

(defn texture-2d-take [texture-id] 
  {:name :texture-2d-take
   :tex-id texture-id 
   :glsl-type :sampler2D})

;to compile a drawer to glsl, you need an init-state!
(defn drawer* [query pipe interface-fill drawcmds-fn]
  {:name "drawer"
   :query query
   :pipe pipe
   :interface-fill interface-fill
   :drawcmds-fn drawcmds-fn})

(defmacro drawer [query-statement pipe interface-fill & drawcmds]
  (let [bound-names (map first (partition 2 2 query-statement))
        state-queries-bind (vec 
                             (apply concat 
                               (map 
                                 (fn [[bind-name q-vec]]
                                  `(~bind-name (retrieve-nested ~'init-state ~q-vec))) 
                                 (partition 2 2 query-statement))))
        interface-fill-fn `(fn [~'init-state]
                             (let ~state-queries-bind
                               ~interface-fill))
        drawcmds-fn `(fn [~'init-state] 
                       ;arg is called init state but actually is current state
                       (let ~state-queries-bind 
                         ~@drawcmds))]
    `(drawer* '~query-statement ~pipe ~interface-fill-fn ~drawcmds-fn)))

(defn drawarrays 
  "directly draw n elements with offset. 
   Supports OpenGLs modes of Interpreting the buffer data.
   (for example :triangles or :triangle-strip)"
  [mode offset n]
  (let [glmode (cond
                 (= mode :triangles) GL_TRIANGLES
                 (= mode :triangle-strip) GL_TRIANGLE_STRIP)] 
    (glDrawArrays glmode offset n)))

(defn draw-elements
  "draw n elements from an index-buffer with offset into it. 
   Supports OpenGLs modes of Interpreting the buffer data.
   (for example :triangles or :triangle-strip)"
  [mode offset n element-buffer]
  (let [glmode (cond
                 (= mode :triangles) GL_TRIANGLES
                 (= mode :triangle-strip) GL_TRIANGLE_STRIP)]
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER element-buffer)
    (glDrawElements glmode n GL_UNSIGNED_INT offset)))

(defn multi-draw-elements
  "draw n meshes with one draw call.
   The element-buffer contains the indices of the vertex-data given
   to the shaders via (buf-take ...)

   offsets is a vector containing the offsets, where to start the nth
   mesh in the element-buffer

   counts is a vector containing the number of indices used from
   offset onwards in the element-buffer.

   This call is equivalent (but much faster) to:
     (doall 
       (map 
         #(draw-elements mode (nth offsets %) (nth counts %) element-buffer) 
         (range n)))

   Supports OpenGLs modes of Interpreting the buffer data.
   (for example :triangles or :triangle-strip)"
  [mode n offsets counts element-buffer]
  (let [glmode (cond
                 (= mode :triangles) GL_TRIANGLES
                 (= mode :triangle-strip) GL_TRIANGLE_STRIP)]
    (glBindBuffer GL_ELEMENT_ARRAY_BUFFER element-buffer)
    (glMultiDrawElements glmode 
                         (into-array Integer/TYPE counts) 
                         GL_UNSIGNED_INT 
                         offsets
                         n)))

(defn texture-2d 
  "create 2D texture with as many mipmap levels as supported"
  [path]
  (let [x-arr (int-array 1)
        y-arr (int-array 1)
        n-arr (int-array 1)
        stbi-buf (org.lwjgl.stb.STBImage/stbi_load path x-arr y-arr n-arr 4)
        _ (.flip stbi-buf)
        texid (glGenTextures)
        _ (glEnable GL_TEXTURE_2D)
        _ (glGetError) ;probably an nvidia driver bug
        _ (glBindTexture GL_TEXTURE_2D texid)
        _ (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 (first x-arr) (first y-arr) 0 GL_RGBA GL_UNSIGNED_BYTE stbi-buf)
        _ (glGenerateMipmap GL_TEXTURE_2D)
        _ (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
        _ (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
        _ (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
        _ (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
        fLargest (glGetFloat org.lwjgl.opengl.EXTTextureFilterAnisotropic/GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT)
        _ (glTexParameterf GL_TEXTURE_2D org.lwjgl.opengl.EXTTextureFilterAnisotropic/GL_TEXTURE_MAX_ANISOTROPY_EXT fLargest)
        _ (org.lwjgl.stb.STBImage/stbi_image_free stbi-buf)]
    texid))

(defn buf-wrap "slow buf" [coll]
  (let [coll-t (type (first coll))
        host-buf (cond 
                   (= coll-t java.lang.Float)
                     (let [b (BufferUtils/createFloatBuffer (count coll))
                           _ (doall (map #(.put b (unchecked-float %)) coll))]
                       b)
                   (= coll-t java.lang.Long)
                     (let [b (BufferUtils/createIntBuffer (count coll))
                           _ (doall (map #(.put b (unchecked-int %)) coll))]
                       b)
                   (= coll-t java.lang.Integer)
                     (let [b (BufferUtils/createIntBuffer (count coll))
                           _ (doall (map #(.put b (unchecked-int %)) coll))]
                       b)
                   (= coll-t java.lang.Byte)
                     (let [b (BufferUtils/createByteBuffer (count coll))
                           _ (doall (map #(.put b (unchecked-byte %)) coll))]
                       b)
                   (= coll-t java.lang.Double)
                     (let [b (BufferUtils/createDoubleBuffer (count coll))
                           _ (doall (map #(.put b (unchecked-double %)) coll))]
                       b)
                   (= coll-t java.lang.Short)
                     (let [b (BufferUtils/createShortBuffer (count coll))
                           _ (doall (map #(.put b (unchecked-short %)) coll))]
                       b))
        _ (.flip host-buf)]
    host-buf))

(defn buf 
  ([coll]
    (let [coll-t (type coll)
          host-buf (cond
                     (instance? java.nio.Buffer coll) coll
                     (= coll-t float-array-type)
                       (-> (BufferUtils/createFloatBuffer (count coll))
                         (.put coll)
                         (.flip))
                     (= coll-t int-array-type)
                       (-> (BufferUtils/createIntBuffer (count coll))
                         (.put coll)
                         (.flip))
                     (= coll-t byte-array-type)
                       (-> (BufferUtils/createByteBuffer (count coll))
                         (.put coll)
                         (.flip))
                     (= coll-t double-array-type)
                       (-> (BufferUtils/createDoubleBuffer (count coll))
                         (.put coll)
                         (.flip))
                     (= coll-t short-array-type)
                       (-> (BufferUtils/createShortBuffer (count coll))
                         (.put coll)
                         (.flip))
                     (sequential? coll) (buf-wrap coll)
                     :auieee (throw 
                               (IllegalArgumentException. 
                                 (str "Wrong type to create buffer. 
                                      Must be native array of 
                                      bytes,ints,shorts,floats or doubles.
                                      Slow, but working is anything that is sequential?
                                      Type was:" coll-t))))
          buf-obj-id (glGenBuffers)
          _ (glBindBuffer GL_ARRAY_BUFFER buf-obj-id)
          _ (glBufferData GL_ARRAY_BUFFER host-buf GL_STATIC_DRAW)
          _ (glBindBuffer GL_ARRAY_BUFFER 0)]
      buf-obj-id))
  ([size address] 
    (let [buf-obj-id (glGenBuffers)
          _ (glBindBuffer GL_ARRAY_BUFFER buf-obj-id)
          _ (nglBufferData GL_ARRAY_BUFFER size address GL_STATIC_DRAW)
          _ (glBindBuffer GL_ARRAY_BUFFER 0)]
      buf-obj-id)))

;state-change fns:
(defn use-shader! [shadername state]
  (glUseProgram (((state :internals) :shaders) shadername)))

; what is "a draw call" conceptually?
; usually a series of glXXX state setting commands
; followed by glDrawArrays/glDrawElements/etc.
; we have the final glDrawXXX and a list of [set-state-fn! argumentlist] tuples
; we reorder them according to their respective :key-vector. 
; These vectors can have user defined keys and internal ones for optimization (state re-use)
; We only traverse and call the diff of the needed state changes
; TODO: is computing ALL the diffs worth it? probably not.

(defmacro at-start! 
  "delays expr-evaluation to start of graphics app where we have an OpenGL context.
  will eval in order, just like def (possible use case)
  Keeps this state in the global-state macro of f3d." 
  [expr]
  `(swap! global-state update-in [:internals :delayed-defs] conj (fn [] ~expr)))

(defn def-delayed-defs! []
  (doall (map #(%) (-> @global-state :internals :delayed-defs))))

(defn add-drawer 
  ([new-drawer state]
    (let [generated-key (keyword (gensym "__drawer__"))] 
      (update-in 
        (assoc-in state [:internals :drawers generated-key] new-drawer) 
        [:internals :not-compiled-drawers-keys] 
        conj 
        generated-key)))
  ([new-drawer key state] 
   (update-in 
     (assoc-in state [:internals :drawers key] new-drawer) 
     [:internals :not-compiled-drawers-keys] 
     conj 
     key)))

(defn add-drawer!
  ([new-drawer]
    (let [generated-key (keyword (gensym "__drawer__"))]
      (swap! global-state 
             (fn [state] 
               (update-in
                 (assoc-in state [:internals :drawers generated-key] new-drawer)
                 [:internals :not-compiled-drawers-keys] 
                 conj
                 generated-key)))
      generated-key))
  ([new-drawer key]
    (swap! global-state 
           (fn [state] 
             (update-in
               (assoc-in state [:internals :drawers key] new-drawer)
               [:internals :not-compiled-drawers-keys]
               conj
               key)))
    key))

(defn add-update-fn! 
  ([new-update-fn]
    (let [generated-key (keyword (gensym "__drawer__"))]
      (swap! global-state 
             (fn [state] 
               (assoc-in state [:internals :update-fns generated-key] new-update-fn)))
      generated-key))
  ([new-update-fn key]
    (swap! global-state 
           (fn [state] 
             (assoc-in state [:internals :update-fns key] new-update-fn)))
    key))

(defn add-update-fn-with-context! 
  ([new-update-fn]
    (let [generated-key (keyword (gensym "__drawer__"))]
      (swap! global-state 
             (fn [state] 
               (assoc-in state [:internals :update-fns-with-context generated-key] new-update-fn)))
      generated-key))
  ([new-update-fn key]
    (swap! global-state 
           (fn [state] 
             (assoc-in state [:internals :update-fns-with-context key] new-update-fn)))
    key))

;TODO: drawer sorting. Ommitting in this version
;only render those, that have a compiled OpenGL program
(defn render! []
  (let [state @global-state]
    (doseq [d (filter :program (-> state :internals :drawers (vals)))] 
      ((:exec-fn d) d state))))

(defn update! []
  (swap! global-state
         (fn [state]
           (reduce #(%2 %1) state (-> state :internals :update-fns (vals))))))

(defn update-with-context! 
  ([] (let [state @global-state]
        (if (-> state :internals :update-with-context-required?)
          (reset! global-state (reduce #(%2 %1) state (-> state :internals :update-fns-with-context (vals)))))))
  ([update?]
    (let [state @global-state]
      (if update?
        (reset! global-state (reduce #(%2 %1) state (-> state :internals :update-fns-with-context (vals))))))))

(defn create-simple-program [vs-source fs-source]
  (let [vs-id (glCreateShader GL_VERTEX_SHADER)
        _ (glShaderSource vs-id vs-source)
        _ (glCompileShader vs-id)
        _ (println "vertex-shader source:\n\n" vs-source "\n")
        _ (println (glGetShaderInfoLog vs-id))
        fs-id (glCreateShader GL_FRAGMENT_SHADER)
        _ (glShaderSource fs-id fs-source)
        _ (glCompileShader fs-id)
        _ (println "fragment-shader source:\n\n" fs-source "\n")
        _ (println (glGetShaderInfoLog vs-id))
        p-id (glCreateProgram)
        _ (glAttachShader p-id vs-id)
        _ (glAttachShader p-id fs-id)
        _ (glLinkProgram p-id)]
        _ (println "link program errors?" (glGetError))
        _ (println (glGetProgramInfoLog p-id))
    p-id))

(defn compile-drawer [drawer]
  (if (:program drawer)
    drawer ;already ret-2-go
    (let [_ (println "drawer does not have a program. COMPILING ...")
          prim-drawer (to-primitives drawer)
          shdrs (compile-glsl prim-drawer)
          program (create-simple-program 
                    (:vertex-shader shdrs)
                    (:fragment-shader shdrs))
          prim-with-program-drawer (assoc prim-drawer
                                          :program
                                          program)
          _ (println "before init fn error state: " (glGetError))
          res ((:init-fn prim-with-program-drawer) prim-with-program-drawer)
          _ (println "after init fn error state: " (glGetError))]
      res
      )))

(defn stop&reset! []
  (swap! global-state assoc :should-stop? true)
  (Thread/sleep 500)
  (reset-global-state!))

(defn init-window! [height width title fullscreen? swapinterval]
  (let [errorCallback (GLFWErrorCallback/createPrint System/err)
        _ (glfwSetErrorCallback errorCallback)
        _ (when-not (glfwInit)
            (throw  (IllegalStateException. "Unable to initialize GLFW")))
        _ (glfwDefaultWindowHints)
        _ (glfwWindowHint GLFW_VISIBLE               GLFW_FALSE)
        _ (glfwWindowHint GLFW_RESIZABLE             GLFW_FALSE)
        _ (glfwWindowHint GLFW_OPENGL_PROFILE        GLFW_OPENGL_CORE_PROFILE)
        _ (glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT GL_TRUE)
        _ (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 4)
        _ (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 5)
        vidmode (glfwGetVideoMode (glfwGetPrimaryMonitor))
        _ (glfwWindowHint GLFW_REFRESH_RATE (.refreshRate vidmode))
        monw (if fullscreen? (.width vidmode) width)
        monh (if fullscreen? (.height vidmode) height)
        mon_id (if fullscreen? (glfwGetPrimaryMonitor) 0)
        window (glfwCreateWindow monw monh title mon_id 0)
        _ (glfwMakeContextCurrent window)
        _ (glfwSwapInterval swapinterval)
        _ (if fullscreen? (glfwSetWindowPos window 0 0) 
                          (glfwSetWindowPos window
                            (/  (-  (.width vidmode) width) 2)
                            (/  (-  (.height vidmode) height) 2)))
        _ (when (= window nil)
            (throw  (RuntimeException. "Failed to create the GLFW window")))
        keyCallback (proxy  [GLFWKeyCallback]  []
                      (invoke [window key scancode action mods]
                        (when (and (= key GLFW_KEY_ESCAPE)
                                   (= action GLFW_RELEASE))
                               (glfwSetWindowShouldClose window true))))
        _ (glfwSetKeyCallback window keyCallback)
        _ (let [vidmode (glfwGetVideoMode (glfwGetPrimaryMonitor))]
              (glfwShowWindow window))]
    (swap! global-state assoc
      :width     monw
      :height    monh
      :title     title
      :angle 0.0
      :last-time  (System/currentTimeMillis)
      :errorCallback errorCallback
      :keyCallback keyCallback
      :window window)))
(defn init-gl! [width height]
  (org.lwjgl.opengl.GL/createCapabilities)
  (println "OpenGL version:" (glGetString GL_VERSION))
  (glClearColor 0.0 0.0 0.0 0.0)
  (glViewport 0 0 width height))
(defn init! "returns window handle" [] 
  (init-window! 1000 1000 "beta" false 0)
  (init-gl! 1000 1000)
  (:window @global-state))
(defn start! 
  ([] (start! identity))
  ([init-fn]
  (let [dyn-drawer-updater 
        (fn [state] 
          (let [not-yet-compiled (-> state :internals :not-compiled-drawers-keys)]
            (assoc-in 
              (assoc-in state [:internals :drawers]
                (reduce (fn [m-drawers k] (update m-drawers k compile-drawer)) 
                        (-> state :internals :drawers)
                        not-yet-compiled))
              [:internals :not-compiled-drawers-keys]
              nil)))]
    (try
      (println "INIT...")
      (println "RUNNING ON THREAD: " )
      (pp (Thread/currentThread))
      (with-local-vars [start-t (System/nanoTime)
                        num-frames 0] 
      (let [update-thread (Thread. (fn [] (while (not (:should-stop? @global-state)) (update!))))
            window-handle (init!)]
        (println "DELAYED DEFS...")
        (def-delayed-defs!)
        (add-update-fn-with-context! dyn-drawer-updater)
        (println "CALLING INIT-FN...")
        (with-glGetError (swap! global-state init-fn))
        (println "ENTERING HOT LOOP...")
        (println "UPDATE WITH CONTEXT...")
        (update-with-context! true)
        (println "UPDATE...")
        (update!)

        (println "init! returned:")
        (pp window-handle)
        (println "STARTING UPDATE THREAD...")
        (.start update-thread)
        (println "ENTERING HOT LOOP...")
        (glDisable GL_CULL_FACE)
        (glEnable GL_DEPTH_TEST)
        (glDepthFunc GL_LEQUAL)
        (while (not (glfwWindowShouldClose window-handle))
          (let [curr-t (System/nanoTime)] 
            (if (> (- curr-t @start-t) 1000000000)
              (do
                (swap! global-state update-in [:internals :fps-stats] conj @num-frames)
                (var-set start-t curr-t)
                (var-set num-frames 0))
              (var-set num-frames (inc @num-frames))))
          (update-with-context!)
          (glClear (bit-or GL_COLOR_BUFFER_BIT  GL_DEPTH_BUFFER_BIT))
          ;(glClearColor 128 128 128 128)
          (render!) ; herer is nullpointer exception
          (glfwSwapBuffers window-handle)
          (glfwPollEvents))
        (println "REQUESTING UPDATE THREAD STOP...")
        (swap! global-state assoc :should-stop? true)
        (.join update-thread 1000)
        (println "DESTROYING CONTEXT & WINDOW...")
        ;actually we dont bother discarding framebuffers,textures,etc.
        ;the driver is a better deallocater than we are. we just play nice with glfw
        (.free (:keyCallback @global-state))
        (.free (:errorCallback @global-state))
        (glfwDestroyWindow (:window @global-state))))

      (finally
        (glfwTerminate)))
    (kisslog "perf.log" (pp (-> @global-state :internals :fps-stats))))))
