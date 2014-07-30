(ns bdd.core
  (:import [java.io Writer])
  (:refer-clojure :exclude [and not or]))


;;-----------------------------------------------------------------------------
;; BDD nodes

(def ^:private terminal-variable (Object.))

(declare node-str)
(declare compare-node)

(defrecord Node [variable low high value]
  Object
  (toString [node] (node-str node))
  Comparable
  (compareTo [node other] (compare-node node other)))

(defn- leaf-node? [node] (identical? (.variable node) terminal-variable))

; Leaves compare "larger" than any internal node so that they end up as actual
; leaves of the tree.
(defn- compare-node
  [n1 n2]
  (if (leaf-node? n1)
    (if (leaf-node? n2)
      0                                ; n1 and n2 are both leaves
      1)                               ; n1 is a leaf, n2 is internal
    (if (leaf-node? n2)
      -1                               ; n1 is internal, n2 is a leaf
      (compare (.variable n1)          ; n1 and n2 are both internal
               (.variable n2)))))


;;-----------------------------------------------------------------------------
;; Rendering formulas

(defn node-str
  [node]
  (if (leaf-node? node)
    (str "<" (.value node) ">")
    (str "(" (.variable node)
         "? " (node-str (.high node))
         ": " (node-str (.low node))
         ")")))

(defmethod print-method Node [node ^Writer writer]
  (.write writer (node-str node)))


;;-----------------------------------------------------------------------------
;; BDDs

(defrecord BDD [nodes])

(defn bdd [] (BDD. (atom {})))

(defn- bdd-node
  ([bdd variable low high value]
   (let [k [variable low high value]]
     (clojure.core/or
       (@(.nodes bdd) k)
       (let [node (Node. variable low high value)]
         (swap! (.nodes bdd) assoc k node)
         node)))))

(defn- leaf
  [bdd value] (bdd-node bdd terminal-variable nil nil value))

(defn- internal
  [bdd variable low high]
  (if (identical? low high)
    low
    (bdd-node bdd variable low high nil)))


;;-----------------------------------------------------------------------------
;; Thread-local "current" BDD

(def ^:dynamic *bdd* nil)

(defmacro with-bdd [bdd & body] `(binding [*bdd* bdd] ~@body))
(defmacro with-new-bdd [& body] `(binding [*bdd* (bdd)] ~@body))


;;-----------------------------------------------------------------------------
;; Creating formulas

(defn f
  ([] (f *bdd*))
  ([bdd] (leaf bdd false)))
(defn t
  ([] (t *bdd*))
  ([bdd] (leaf bdd true)))

(defn variable
  ([v] (variable *bdd* v))
  ([bdd v] (internal bdd v (f bdd) (t bdd))))
(defn not-variable
  ([v] (not-variable *bdd* v))
  ([bdd v] (internal bdd v (t bdd) (f bdd))))


;;-----------------------------------------------------------------------------
;; Using formulas

(defn all-where
  [value node]
  (if (leaf-node? node)
    (if (= value (.value node)) [{}] [])
    (let [variable (.variable node)]
      (lazy-cat (map #(assoc % variable false)
                     (all-where value (.low node)))
                (map #(assoc % variable true)
                     (all-where value (.high node)))))))


; We don't use recur in the body of these functions because we want recursive
; calls to pick up memoized values.  We need the Y-combinator trick of passing
; in the function itself as a parameter since you can't use the name of a let
; binding recursively in its definition [1].
;
; [1] http://stackoverflow.com/a/12955191


(defn- apply1
  [bdd op lhs]
  (let [internal-apply1
        (memoize
          (fn [recur' lhs]
            (let [lhs-var (.variable lhs)]
              (if (leaf-node? lhs)
                (leaf bdd (op (.value lhs)))
                (let [low (recur' recur' (.low lhs))
                      high (recur' recur' (.high lhs))]
                  (internal bdd lhs-var low high))))))]
    (internal-apply1 internal-apply1 lhs)))

(defn- apply2
  [bdd op lhs rhs]
  (let [internal-apply2
        (memoize
          (fn [recur' lhs rhs]
            (case (compare lhs rhs)
              0  (if (leaf-node? lhs)
                   (leaf bdd (op (.value lhs) (.value rhs)))
                   (let [low (recur' recur' (.low lhs) (.low rhs))
                         high (recur' recur' (.high lhs) (.high rhs))]
                     (internal bdd (.variable lhs) low high)))
              -1 (let [low (recur' recur' (.low lhs) rhs)
                       high (recur' recur' (.high lhs) rhs)]
                   (internal bdd (.variable lhs) low high))
              1  (let [low (recur' recur' lhs (.low rhs))
                       high (recur' recur' lhs (.high rhs))]
                   (internal bdd (.variable rhs) low high)))))]
    (internal-apply2 internal-apply2 lhs rhs)))

(defn not
  ([lhs] (not *bdd* lhs))
  ([bdd lhs] (apply1 bdd #(clojure.core/not %1) lhs)))

(defn and
  ([lhs rhs] (and *bdd* lhs rhs))
  ([bdd lhs rhs] (apply2 bdd #(clojure.core/and %1 %2) lhs rhs)))

(defn or
  ([lhs rhs] (or *bdd* lhs rhs))
  ([bdd lhs rhs] (apply2 bdd #(clojure.core/or %1 %2) lhs rhs)))
