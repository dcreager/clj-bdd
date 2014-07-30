(ns bdd.core-test
  (:require [clojure.test :refer :all]
            [bdd.core :as bdd
                      :refer :all :exclude [and not or]]))


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


(deftest test-all-where
  (testing "all-where"
    (with-new-bdd
      (let [b (variable :b)
            nc (not-variable :c)
            x (@#'bdd.core/internal *bdd* :a nc b)]

        (is (= (all-where true (t)) [{}]))
        (is (= (all-where false (t)) []))

        (is (= (all-where true (f)) []))
        (is (= (all-where false (f)) [{}]))

        (is (= (all-where true b) [{:b true}]))
        (is (= (all-where false b) [{:b false}]))

        (is (= (all-where true nc) [{:c false}]))
        (is (= (all-where false nc) [{:c true}]))

        ; The order is deterministic, since it's based on the variable ordering,
        ; and the fact that we recurse into the low branch first.
        (is (= (all-where true x) [{:a false :c false}
                                   {:a true :b true}]))
        (is (= (all-where false x) [{:a false :c true}
                                    {:a true :b false}]))
      )
    )
  )
)

(deftest test-operators
  (testing "operators"
    (with-new-bdd

      (is (identical? (bdd/not (variable :a)) (not-variable :a)))

      (is (identical? (bdd/and (variable :a) (variable :b))
                      (@#'bdd/internal *bdd* :a
                                       (f)
                                       (@#'bdd/internal *bdd* :b (f) (t)))))

      (is (identical? (bdd/or (variable :a) (variable :b))
                      (@#'bdd/internal *bdd* :a
                                       (@#'bdd/internal *bdd* :b (f) (t))
                                       (t))))
    )
  )
)
