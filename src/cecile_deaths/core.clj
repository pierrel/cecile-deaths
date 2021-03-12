(ns cecile-deaths.core)

(defn simulate-events [size]
  (loop [remaining size
         event-slots (vec (take size (repeat 0)))
         iterations 0
         current-event-slot (rand-int size)]
    (if (zero? remaining)
      {:iterations iterations
       :event-slots event-slots}
      (recur (if (zero? (nth event-slots current-event-slot))
               (dec remaining)
               remaining)
             (update-in event-slots (vector current-event-slot) inc)
             (inc iterations)
             (rand-int size)))))

(defn multi-sim [runs size]
  (let [empty-results (take runs (repeat 0))]
    (map (fn [_]
           (:iterations (simulate-events size)))
         empty-results)))

(defn plot-out [col]
  (clojure.string/join "\n"
                       (map (fn [[x y]]
                              (format "%d %d" x y))
                            (partition 2
                                       (interleave (iterate inc 1)
                                                   col)))))

(defn -main [& args]
  (let [runs (or (-> args first Integer/parseInt) 10)
        size (or (-> args second Integer/parseInt) 700)]
    (println (plot-out (multi-sim runs size)))))
