(ns datamunging.weatherdata)

; From http://codekata.pragprog.com/2007/01/kata_four_data_.html
;
; Kata Four: Data Munging
;
; Part One: Weather Data
;
; In weather.dat you’ll find daily weather data for Morristown, NJ for June 2002. 
; Download this text file, then write a program to output the day number (column one) 
; with the smallest temperature spread (the maximum temperature is the second 
; column, the minimum the third column).

(def wheather-data-file-name "weather.dat")

(def file-lines (line-seq (clojure.java.io/reader wheather-data-file-name)))

(defn non-asterisk? [character] (not (= \* character)))
(defn is-space? [character] (= \space character))
(defn is-digit? [character] (contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} character))

(defn strip-asterisks [line] (filter non-asterisk? line))

(def cleaned-lines (map strip-asterisks file-lines))

(defn strip-leading-spaces [string] (drop-while is-space? string))

(defn starts-with-digit [line] (is-digit? (first (strip-leading-spaces line))))

(defn is-data-line? [line] (starts-with-digit line))

(def data-lines (filter is-data-line? cleaned-lines))

(defn extract-digits [text] (take-while is-digit? (strip-leading-spaces text)))
(defn drop-digits [text] (drop-while is-digit? (strip-leading-spaces text)))

(defn as-integer [characters] (java.lang.Integer/parseInt (apply str characters)))

(defn extract-numbers [count text]
  (if (= count 0) 
    ()
    (let [digits (extract-digits text)
          number (as-integer digits)
          rest-of-text (drop-digits text)]
      (cons number 
            (extract-numbers (dec count) rest-of-text)))))

(defn extract-excursion-details [line] 
  (let [[day maxtemp mintemp] (extract-numbers 3 line)]
    {:day day :excursion (- maxtemp mintemp)}))

(def line-reading-details (map extract-excursion-details data-lines))

(defn excursion-comparator [{day1 :day excursion1 :excursion } {day2 :day excursion2 :excursion }]
  (cond (> excursion1 excursion2) -1
        (= excursion1 excursion2) 0
        :else 1))

(def line-reading-details-by-descending-excursion (sort excursion-comparator line-reading-details ))

(def dates-by-descending-excursions (map :day line-reading-details-by-descending-excursion))

(def day-with-greatest-excursion (first dates-by-descending-excursions )) 

(println "the day with the greatest excursion is" day-with-greatest-excursion)
