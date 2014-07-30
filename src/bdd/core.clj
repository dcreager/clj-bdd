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
;; Creating formulas

(defn f [bdd] (leaf bdd false))
(defn t [bdd] (leaf bdd true))

(defn variable     [bdd variable] (internal bdd variable (f bdd) (t bdd)))
(defn not-variable [bdd variable] (internal bdd variable (t bdd) (f bdd)))
