(ns CDExperiment.core
    (:use [CDExperiment.CDParser]))

(defn -main [& args]
    (def s1 (parse-experiment "/Users/biomunky/scratch/CDExperiment/1a8e.pdb" :gen))
    (def s2 (parse-experiment "/Users/biomunky/scratch/CDExperiment/aviv1.txt" :aviv))
    (def s3 (parse-experiment "/Users/biomunky/scratch/CDExperiment/jasco.txt" :jasco))
    (def s4 (parse-experiment "/Users/biomunky/scratch/CDExperiment/bp2.txt" :bp))
;    (println (:meta s1))
;    (println (:meta s2))
;    (println (:meta s3))
    (println (:meta s4))
    (flush))
