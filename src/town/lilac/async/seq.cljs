(ns town.lilac.async.seq
  (:refer-clojure :exclude [cons first next])
  (:require-macros [town.lilac.async.seq :refer [lazy-aseq]]))


(defprotocol IAsyncSeq
  (await-first [as])
  (await-next [as]))


(defprotocol IAsyncSeqable
  (aseq [x]))


(extend-type nil
  IAsyncSeqable
  (aseq [_] (js/Promise.resolve nil)))


(deftype AsyncLazySequence [thunk ^:mutable cached-aseq]
  IPending
  (-realized? [_] (some? cached-aseq))
  IAsyncSeqable
  (aseq [_]
    (if cached-aseq
      cached-aseq
      (doto (let [s (or cached-aseq (thunk))]
              (if (instance? AsyncLazySequence s)
                (aseq s)
                s))
        (->> (set! cached-aseq)))))
  IAsyncSeq
  (await-first [this]
    (.then (aseq this) await-first))
  (await-next [this]
    (.then (aseq this) await-next)))


(deftype AsyncCons [head next]
  IAsyncSeqable
  (aseq [this] (js/Promise.resolve this))
  IAsyncSeq
  (await-first [_] (js/Promise.resolve head))
  (await-next [_] (aseq next)))


(defn cons
  ([v thunk]
   (->AsyncCons v thunk)))


(defn first [aseq] (await-first aseq))


(defn next [aseq] (await-next aseq))


(defn transmute
  ([xform f src] (transmute xform f (f) src))
  ([xform f init src]
   (let [rf (xform f)
         step
         (fn step [results iter]
           (-> (aseq iter)
               (.then (fn [x]
                        (if (some? x)
                          (.then (first x)
                                 #(let [results (rf results %)]
                                    (if (reduced? results)
                                      (unreduced results)
                                      (.then (next x) (partial step results)))))
                          results)))))]
     (step init src))))



(defn collect
  ([to from] (transmute identity conj to from))
  ([to xform from] (transmute xform conj to from)))


(defn iteration
  [step & {:keys [somef vf kf initk]
           :or {somef some?
                vf identity
                kf identity}}]
  (lazy-aseq
   ((fn do-next [p]
      (.then p (fn [ret]
                 (when (somef ret)
                   (cons
                    (vf ret)
                    (when-some [k (kf ret)]
                      (lazy-aseq (do-next (step k)))))))))
    (step initk))))


#_(-> (cons
     (js/Promise.resolve 1)
     #(cons (js/Promise.resolve 2) nil))
    (next)
    (.then first)
    (.then prn))


#_(-> (->AsyncLazySequence
     (fn []
       (cons
        (js/Promise.resolve 1)
        (->AsyncLazySequence
         (fn []
           (cons (js/Promise.resolve 2) nil))
         nil)))
     nil)
    (next)
    #_(.then first)
    (.then next)
    (.then prn))


#_(lazy-aseq
   (cons
    (js/Promise.resolve 1)
    nil))
