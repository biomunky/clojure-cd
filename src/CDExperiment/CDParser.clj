(ns CDExperiment.CDParser
    (:use [clojure.java.io :only [reader]]))

(defrecord Experiment [meta wavelength spectrum])

(defn -extract-xy
  "private method to extract x/yvals. Takes a string and splits it
  returns [xval yval]"
  [line ycol]
  (let [values (vec (.split #"\s+|\t+" line))]
    [(Float/parseFloat (first values)) (Float/parseFloat (nth values ycol))]))

; this will return ''/empty strings for some metadata types
;in some of the filetypes
(defn -extract-meta
  "private method to parse metadata keys and values.  Takes a string and
   returns [key value]"
  [line pattern]
  (let [ [k v] (vec (.split pattern line 2)) ]
    [(.trim k) (.trim v)]))

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
	(if (empty? content) (Experiment. metadata xvals yvals)
	    (cond
					; first get the lines that start with numbers
	     (re-find #"^\d+\s+\d.*" (first content)) (let [[x y] (-extract-xy (first content) col)]
							(recur (rest content)
							       metadata
							       (cons x xvals)
							       (cons y yvals)))
					; second up, look for meta data
	     (re-find #"^\w+\s+\w{1,}" (first content)) (let [[k v]
							      (-extract-meta (first content) meta-split-pattern)]
							  (recur (rest content)
								 (assoc metadata k v)
								 xvals
								 yvals))
	     :else (recur (rest content) metadata xvals yvals) )))))

(defn strings-to-map
  "Takes two strings - s1 headers s2 values
assumes that s1 and s2 have the same number of elts
when split"
  [s1 s2]
  (zipmap (.split #"\s{2,}" (.trim s1)) (.split #"\s{2,}" (.trim s2))))

(defn parse-bp
  "A parser for bp files, specific version of parse-file.
 Only requires fully qualified file"   
  [filename]
  (with-open [rdr (reader filename)]
    (loop [content (line-seq rdr) metadata {} xvals [] yvals []]
      (if (empty? content) (Experiment. metadata xvals yvals)
	  (let [line (first content) ]
	    (cond
	     (re-matches #".*LOW LAMBDA.*|.*CONC.*|.*TIME.*" line) (let [next-line (second content)]
								     (recur (drop 2 content)
									    (merge metadata (strings-to-map line next-line))
									    xvals
									    yvals))
	     
	     (re-matches #"^\s+\d+.*" line) (let [ [x y] (-extract-xy (.trim line) 1)]
					      (recur (rest content)
						     metadata
						     (cons x xvals)
						     (cons y yvals)))
	     
	     :else (recur (rest content) metadata xvals yvals)))))))

(defn parse-experiment 
  "Parses a file (filename) containing output from a CD experiment
    file-type is a symbol of 'gen 'jasco 'aviv.  Delegates to another
    function to parse specific filetypes
    " 
  [filename file-type]
  (cond
   (= file-type :gen)   (generic-file-parser filename 3 #"\t{1,}" )
   (= file-type :aviv)  (generic-file-parser filename 1 #"\s+")
   (= file-type :jasco) (generic-file-parser filename 1 #"\t+")
   (= file-type :plain) (generic-file-parser filename 1 #"\s+")
   (= file-type :bp)    (parse-bp filename)
   :else (println "Got something else")))




