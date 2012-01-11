(ns cd.core
    (:gen-class)
    (:use [cd.parser]
	  [cd.experiment]))

(defn -main [& args]
    (def s1 (parse-experiment "resources/gen.txt" :gen))
    (def s2 (parse-experiment "resources/aviv1.txt" :aviv))
    (def s3 (parse-experiment "resources/jasco.txt" :jasco))
    (def s4 (parse-experiment "resources/bp2.txt" :bp))

    (write-gen s1 "/tmp/mygen.txt")
    
    (def s5 (parse-experiment "resources/pcd.txt" :pcd))
    (println (keys s5))
    (println (:metadata s5))    
    
    (flush))