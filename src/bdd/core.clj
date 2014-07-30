(ns bdd.core)


;;-----------------------------------------------------------------------------
;; BDD nodes

(def ^:private terminal-variable (Object.))

(defrecord Node [variable low high value])

(defn- leaf-node? [node] (identical? (.variable node) terminal-variable))


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
