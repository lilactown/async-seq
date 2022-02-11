(ns user
  (:require
   [kitchen-async.promise :as p]
   [town.lilac.async.seq :as aseq]))


;;
;; Example of an async API for fetching pages of data, where the token in the
;; payload tells the consumer how to fetch the next page
;;

(def ^:private pages-data
  "Some example page data, keyed by the `:page`.
  The next page is contained in the payload at the `:next` key."
  (into {} (map (juxt :page identity)) (for [n (range 10)]
                                         {:page n
                                          :next (inc n)})))


(defn- fetch-page
  "Fetch a particular page"
  [n]
  (prn "Fetching " n)
  (js/Promise.resolve (get pages-data n)))



(defn simple-example
  []
  (let [pages (aseq/iteration fetch-page
                              :initk 0
                              :kf :next)]
    (-> pages
        (aseq/aseq)
        (.then aseq/next)
        (.then aseq/next)
        (.then aseq/first))))

#_(.then (simple-example) prn)


(defn recursive-example
  ([n]
   (let [pages (aseq/iteration fetch-page
                               :initk 0
                               :kf :next)]
     (.then
      (aseq/first pages)
      #(recursive-example (dec n) [%] (aseq/next pages)))))
  ([n results p]
   (if (pos? n)
     (.then p (fn [aseq]
                (if aseq
                  (.then
                   (aseq/first aseq)
                   #(recursive-example
                     (dec n) (conj results %)
                     (aseq/next aseq)))
                  results)))
     results)))

#_(.then (recursive-example 4) prn)


(defn transduce-example
  []
  (aseq/transduce
   (comp (take 3) (map :page))
   conj []
   (aseq/iteration fetch-page
                   :initk 0
                   :kf :next)))

#_(.then (transduce-example) prn)


(defn collect-example
  []
  (aseq/collect [] (comp (take 3) (map :page)) (aseq/iteration fetch-page
                                                               :initk 0
                                                               :kf :next)))

#_(.then (collect-example) prn)


(defn loop-example
  []
  (p/loop [pages (aseq/iteration fetch-page
                                 :initk 0
                                 :kf :next)
           results []
           page-num 6]
    (p/let [page (aseq/first pages)]
      (if (and (pos? page-num) (some? page))
        (p/recur
         (aseq/next pages)
         (conj results page)
         (dec page-num))
        results))))

#_(.then (loop-example) prn)
