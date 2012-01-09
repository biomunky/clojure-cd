(ns cd.test.experiment
  (:use [clojure.test]
        [cd.experiment]))
        
(deftest test-mre-to-de
    (is (= (mre-to-de 3298.0) 1.0)))

(deftest test-machine-units-to-de
    (is (= (machine-units-to-de 3298.0 1.0 1.0 10.0) 1.0)))
    
(deftest test-yy-units-to-de
    (is (= (yy-to-de 3298.0 1.0 1.0 1.0) 10.0)))
