(ns cd.experiment
    (:use [clojure.java.io :only [writer]]))

(defrecord Experiment [metadata wavelength spectrum ht baseline calibration])

(defn experiment-record 
  "Creates an Experiment record - checks first to make sure the 
  wavelengh/spectrum data runs from low to high"
  [meta wavelength spectrum ht base cali]
  (if (> (first wavelength) (second wavelength) )
    (Experiment. meta (reverse wavelength) (reverse spectrum) (reverse ht) (reverse base) (reverse cali))
    (Experiment. meta wavelength spectrum ht base cali)))

(defn mre-to-de [x] (/ x 3298.0))

(defn machine-units-to-de [x pathlength concentration mrw]
    (* x (/ (* 0.1 mrw) (* (* pathlength concentration) 3298.0)))) 

(defn yy-to-de [x pathlength concentration mrw]
    (* (* x 100.0) (/ (* 0.1 mrw) (* (* pathlength concentration) 3298.0) )))
    
(defn convert-mre-to-de
  "Converts a seq of numbers from mean residue ellipticity to de.
Requires pathlength in cm, concentration mg/ml and mean residue weight"
  [lst]
    (map #(mre-to-de %) lst))
    
(defn convert-machine-units-to-de
  "Converts a seq of numbers from machine units to de.
Requires pathlength in cm, concentration mg/ml and mean residue weight"
  [lst pathlength concentration mrw]
    (map #(machine-units-to-de % pathlength concentration mrw) lst))
    
(defn convert-yy-to-de
  "Converts a seq of numbers from yy to de.  Requires pathlength in cm, concentration
mg/ml and mean residue weight"
  [lst pathlength concentration mrw]
    (map #(yy-to-de % pathlength concentration mrw) lst))

(defn meta-string
  "Takes an Experiment record and returns the metadata as a string."
  [experiment]
  (let [m (:meta experiment)]
    (apply str (interpose "\n" (map #(format "%-23s %s" %1 %2) (keys m) (vals m))))))

(defn data-string
  "Takes an Experiment. and produces a string that's supposed to behave like a gen"
  [experiment]
  (let [s (:spectrum experiment) w (:wavelength experiment)]
    (apply str (interpose "\n"
			  (map #(format "%1.1f	   % 2.5E	   % 2.5E	   % 2.5E	    0.00000E+00	    0.00000E+00	    0.00000E+00" %1 %2 %2 %2) w s)))))

(defn print-gen [experiment]
  (apply str (interpose "\n" [(meta-string experiment) (data-string experiment)])))

(defn write-gen [experiment outputfile]
    (let [g (print-gen experiment)]
        (try
            (with-open [wtr (writer outputfile)]
            (.write wtr g))
            (catch Exception e (throw e)))))
