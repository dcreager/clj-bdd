(ns bdd.core-test
  (:require [clojure.test :refer :all]
            [bdd.core :refer :all]))


(deftest test-bdd-nodes-explicit
  (testing "bdd nodes with explicit parameters"
    (let [bdd (bdd)]

      (is (= (:value (f bdd)) false))
      (is (= (:value (t bdd)) true))

      (is (identical? (f bdd) (f bdd)))
      (is (identical? (t bdd) (t bdd)))
      (is (not (identical? (f bdd) (t bdd))))

      (is (identical? (:low (variable bdd :x)) (f bdd)))
      (is (identical? (variable bdd :x) (variable bdd :x)))

      (is (identical? (@#'bdd.core/internal bdd :a (f bdd) (f bdd))
                      (f bdd)))
      (is (identical? (@#'bdd.core/internal bdd :a (f bdd) (t bdd))
                      (@#'bdd.core/internal bdd :a (f bdd) (t bdd))))
    )
  )
)


(deftest test-bdd-nodes
  (testing "bdd nodes"
    (with-new-bdd

      (is (= (:value (f)) false))
      (is (= (:value (t)) true))

      (is (identical? (f) (f)))
      (is (identical? (t) (t)))
      (is (not (identical? (f) (t))))

      (is (identical? (:low (variable :x)) (f)))
      (is (identical? (variable :x) (variable :x)))
    )
  )
)


(deftest test-compare-nodes
  (testing "comparing nodes"
    (with-new-bdd
      (is (compare (f) (f)) 0)
      (is (compare (t) (t)) 0)
      (is (compare (f) (t)) 0)
      (is (compare (t) (f)) 0)

      (is (compare (variable :a) (variable :b)) -1)
      (is (compare (variable :a) (variable :a))  0)
      (is (compare (variable :b) (variable :a))  1)

      (is (compare (variable :a) (f)) -1)
      (is (compare (f) (variable :a))  1)
    )
  )
)
