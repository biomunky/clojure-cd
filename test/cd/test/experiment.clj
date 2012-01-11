(ns cd.test.experiment
  (:require cd.experiment)
  (:import [cd.experiment Experiment])    
  (:use [clojure.test]
        [cd.experiment]
        [cd.parser]
        midje.sweet))
        
(fact (mre-to-de 3298.0) => 1.0)

(fact (machine-units-to-de 3298.0 1.0 1.0 10.0) => 1.0)
    
(fact (yy-to-de 3298.0 1.0 1.0 1.0) => 10.0)

(def my-test (Experiment. {"key1" "val1", "key2" "val2"}
                      [1, 2, 3, 4]
                      [5, 6, 7, 8] () () ()))

(deftest test-experiment-record
    (is (= (:metadata my-test) {"key1" "val1", "key2" "val2"}))
    (is (= (:wavelength my-test) [1 2 3 4]))
    (is (= (:spectrum my-test) [5 6 7 8])))

(deftest test-experiment-record-reorder
    (def tset-ym (experiment-record 
                    {"key1" "val1", "key2" "val2"}
                    (reverse [1, 2, 3, 4])
                    (reverse [5, 6, 7, 8])
                    () () ()))
    (is (= my-test tset-ym)))


(deftest test-write-gen-exception
    (def exp (parse-experiment "resources/gen.txt" :gen))
    (is (thrown? Exception (write-gen exp "/tmpa/test.gen"))))

(deftest test-write-gen-success
    (def exp (parse-experiment "resources/gen.txt" :gen))
    (is (nil? (write-gen exp "/tmp/test.gen"))))