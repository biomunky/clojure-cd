(ns cd.parser
    (:require cd.experiment)
    (:import [cd.experiment Experiment])
    (:use [clojure.java.io :only [reader]]
          [clojure.string :only [trim split]]
          [cd.experiment]))

(defn -extract-xy
  "private method to extract x/yvals. Takes a string and splits it
  returns [xval yval]"
  [line ycol]
  (let [values (vec (split line #"\s+|\t+"))]
    [(Float/parseFloat (first values)) (Float/parseFloat (nth values ycol))]))

; this will return ''/empty strings for some metadata types
;in some of the filetypes
(defn -extract-meta
  "private method to parse metadata keys and values.  Takes a string and
   a pattern to split on 
   -> returns [key value]"
  [line pattern]
  (let [ [k v] (vec (split line pattern 2)) ]
    [(trim k) (trim v)]))

(defn strings-to-map
  "Takes two strings - s1 headers s2 values
assumes that s1 and s2 have the same number of elts
when split - uses two or more w/s chars to split"
  [s1 s2]
  (zipmap (split (trim s1) #"\s{2,}")
	  (split (trim s2) #"\s{2,}")))

(defn bp-parser
  "A parser for bp files, specific version of parse-file.
 Only requires fully qualified file"   
  [filename]
  (with-open [rdr (reader filename)]
    (loop [content (line-seq rdr) metadata {} xvals [] yvals []]
      (if (empty? content) (experiment-record metadata xvals yvals nil nil nil)
	  (let [line (first content) ]
	    (cond
	     (re-matches #".*LOW LAMBDA.*|.*CONC.*|.*TIME.*" line) (let [next-line (second content)]
								     (recur 
								        (drop 2 content)
									    (merge metadata (strings-to-map line next-line))
									    xvals
									    yvals))	     
	     (re-matches #"^\s+\d+.*" line) (let [ [x y] (-extract-xy (trim line) 1)]
					      (recur 
					         (rest content)
						     metadata
						     (cons x xvals)
						     (cons y yvals)))	     
	     :else (recur (rest content) metadata xvals yvals)))))))

(defn update-hashmap [m ks vs]
   (reduce (fn [m [k v]] (assoc m k (conj (m k) v)))
 	  m (zipmap ks vs)))

(defn pcd-parser [filename]
  (with-open [rdr (reader filename)]
    (loop [content (line-seq rdr)
	   headers {}
	   data { :spectrum [] :ht [] :wavelength [] :baseline []}
	   in-data false
	   in-calibration false]
      (if (empty? content) (experiment-record headers 
                                              (data :wavelength) 
                                              (data :spectrum)
                                              (data :ht)
                                              (data :baseline)
                                              nil )
	  (let [current-line (first content)]
	    (cond
	     (re-find #"PCDDB DATA FILE" current-line) (recur (rest content)
							      headers
							      data
							      in-data
							      in-calibration)
	     
	     (re-find #"^DATA \(" current-line) (recur (rest content)
						       headers
						       data
						       true
						       in-calibration)
	     
	     ; screw the calibration - it's made up anyway (so's the baseline & ht)
	     (re-find #"^CALIBRATION" current-line) (recur (rest content)
							   headers
							   data
							   false
							   true)
	     (true? in-calibration) (recur (rest content)
					   headers
					   data
					   in-data
					   in-calibration)
	     
	     (true? in-data) (let [ [wl spt ht _ _ base] (split current-line #"\s+")]
			       (recur (rest content)
				      headers
				      (update-hashmap data
						      [:spectrum :wavelength :ht :baseline]
						      (map #(Float/parseFloat %) [spt wl ht base]))
				      in-data
				      in-calibration))
	     
	     :else (recur
		    (rest content)
		    (let [ [k v] (map trim (split current-line #"\s{2,}")) ]
		      (assoc headers k v))
		    data
		    in-data
		    in-calibration)))))))
		    
(defn generic-file-parser
     "Parses gen, jasco and aviv files.
     Args: file -> full path/string
           col -> int 0 indexed column containing spectra
           pattern -> upon which the metadata is split
     Creates an Experiment record -> keys metadata xvals yvals
     "
     [filename col meta-split-pattern]
     (with-open [rdr (reader filename)]
       (loop [content (line-seq rdr) 
 	     metadata {}
 	     xvals [] 
 	     yvals []]
 	(if (empty? content) (experiment-record metadata xvals yvals nil nil nil)
 	    (cond
 	     (re-find #"^\d+.*\s.*" (first content)) (let [[x y] (-extract-xy (first content) col)]
						       (recur (rest content)
							      metadata
							      (cons x xvals)
							      (cons y yvals)))
 	     (re-find #"^\w+\s+\w{1,}" (first content)) (let [[k v]
 							      (-extract-meta (first content) meta-split-pattern)]
 							  (recur (rest content)
 								 (assoc metadata k v)
 								 xvals
 								 yvals))
 	     :else (recur (rest content) metadata xvals yvals) )))))

(defn parse-experiment 
  "Parses a file (filename) containing output from a CD experiment
    file-type is a keyword of :gen :jasco :aviv.  Delegates to another
    function to parse specific filetypes
    " 
  [filename file-type]
  (cond
   (= file-type :gen)   (generic-file-parser filename 3 #"\t{1,}" )
   (= file-type :aviv)  (generic-file-parser filename 1 #"\s+")
   (= file-type :jasco) (generic-file-parser filename 1 #"\s+")
   (= file-type :plain) (generic-file-parser filename 1 #"\s+")
   (= file-type :pcd)   (pcd-parser filename)   
   (= file-type :bp)    (bp-parser filename)
   :else (println "Got something else"))) ;this should throw something




