(ns bdd.core
  (:import [java.io Writer]))


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
     (or
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
