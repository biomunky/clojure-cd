(ns cd.test.parser
  (:require cd.experiment)
  (:import [cd.experiment Experiment])
  (:use [clojure.test]
        [cd.parser]))

(def my-test (Experiment. {"key1" "val1", "key2" "val2"}
                      [1, 2, 3, 4]
                      [5, 6, 7, 8]))

(def my-meta-a "Key     Value")
(def my-meta-b "Key Value")
(def my-meta-c "Key Value Value")

(def test-keys "a  b  c  d  e")
(def test-values "1  2  3  4  5")

(deftest test-experiment-record
    (is (= (:meta my-test) {"key1" "val1", "key2" "val2"}))
    (is (= (:wavelength my-test) [1 2 3 4]))
    (is (= (:spectrum my-test) [5 6 7 8])))

(deftest test-experiment-record-reorder
    (def tset-ym (experiment-record 
                    {"key1" "val1", "key2" "val2"}
                    (reverse [1, 2, 3, 4])
                    (reverse [5, 6, 7, 8])))
    (is (= my-test tset-ym)))

(deftest test-extract-xy
    (is (= (-extract-xy "123\t123.0" 1)      [123.0, 123.0] )))

(deftest test-extract-meta
    (is (= (-extract-meta my-meta-a #"\s+") ["Key" "Value"] ))
    (is (= (-extract-meta my-meta-b #"\s+") ["Key" "Value"] ))
    (is (= (-extract-meta my-meta-c #"\s+") ["Key" "Value Value"] )))
    
(deftest test-strings-to-map
    (is (= (strings-to-map test-keys test-values) 
        {"a" "1", "b" "2", "c" "3", "d" "4", "e" "5"} )))
        
(deftest read-gen
    (def real-case (parse-experiment "test/cd/resources/gen.txt" :gen))
    (def test-case (experiment-record {"Date" "2008-05-06T00:00:00",
                        "Generic" "a24703.gen",
                        "Experiment" "Transferrin"}
                        '(279.0 280.0) '(2.0 1.0)))
    (is (= (:meta real-case) (:meta test-case)))
    (is (= (:wavelength real-case) (:wavelength test-case)))
    (is (= (:spectrum real-case) (:spectrum test-case))))    

(deftest read-jasco
    (def real-case (parse-experiment "test/cd/resources/jasco.txt" :jasco))
    (def test-case (experiment-record {"ORIGIN" "JASCO", 
                                       "DATA" "TYPE", 
                                       "TITLE" "WT-TTR. Gdn Unfold. 990710"}
                        '(429.5 430.0) '(-2.0 -1.0)))
    (is (= (:meta real-case) (:meta test-case)))
    (is (= (:wavelength real-case) (:wavelength test-case)))
    (is (= (:spectrum real-case) (:spectrum test-case))))

(deftest read-aviv
    (def real-case (parse-experiment "test/cd/resources/aviv1.txt" :aviv))
    (def test-case (experiment-record {"data_name" "b030600b.001",
                                       "_err_strngncy_" "3",
                                       "_error_filter_"  "10"}
                        '(279.0 280.0) '(-2.00 -1.00)))
    (is (= (:meta real-case) (:meta test-case)))
    (is (= (:wavelength real-case) (:wavelength test-case)))
    (is (= (:spectrum real-case) (:spectrum test-case))))
     
(deftest read-bp
    (def real-case (parse-experiment "test/cd/resources/bp2.txt" :bp))
    (def test-case (experiment-record {"DETECTOR POSITION" "0.00", 
                                       "SCAN SPEED(nm/min)" "0.00",
                                       "MEAN RES MW" "0.000", 
                                       "PATHLENGTH (cm)" "0.000",
                                       "TIME CONST (s)" "0.00",
                                       "LOW LAMBDA (nm)" "160.0",
                                       "# OF POINTS" "501",
                                       "DELTA X" "0.0", 
                                       "SCALE (Deg)" "0.000",
                                       "# SCANS" "08",
                                       "HIGH LAMBDA (nm)" "260.0",
                                       "CONC (mg/ml)" "0.000E+00"} 
                        '(160.0 161.0) '(-10000.00 -20000.00)))
    (is (= (:meta real-case) (:meta test-case)))
    (is (= (:wavelength real-case) (:wavelength test-case)))
    (is (= (:spectrum real-case) (:spectrum test-case))))
    
(deftest read-plain
    (def real-case (parse-experiment "test/cd/resources/plain.txt" :plain))
    (def test-case (experiment-record {}
                        '(1.0 2.0 3.0) '(1.0 2.0 3.0)))
    (is (= (:meta real-case) (:meta test-case)))
    (is (= (:wavelength real-case) (:wavelength test-case)))
    (is (= (:spectrum real-case) (:spectrum test-case))))    
    
    