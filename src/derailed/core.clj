(ns derailed.core)

(def zemun {:id :zemun :name "Zemun" :offset 10})

(def stations
  [{:id :batajnica :name "Batajnica" :offset 0}
   {:id :zemun :name "Zemun" :offset 10}
   {:id :novi-beograd :name "Novi Beograd" :offset 16}
   {:id :vukov-spomenik :name "Vukov Spomenik" :offset 26}
   {:id :ovca :name "Ovča" :offset 41}])

(def train {:number 8001 :departs 370})

(def timetable
  [train
   {:number 8003 :departs 400}
   {:number 8005 :departs 430}])

(def report {:user "marko"
             :train 8001
             :type :seen
             :station :zemun
             :at 384})

(def reports
  [report
   {:user "ana" :train 8001 :type :seen :station :novi-beograd :at 391}
   {:user "troll" :train 8001 :type :seen :station :zemun :at 540}
   {:user "jelena" :train 8003 :type :cancelled :station :batajnica :at 398}
   {:user "nikola" :train 8003 :type :cancelled :station :zemun :at 405}])

(defn scheduled-time
  [train station]
  (+ (:departs train) (:offset station)))

(defn report-delay
  [report train station]
  (- (:at report) (scheduled-time train station)))

(defn average
  [numbers]
  (quot (reduce + numbers) (count numbers)))

(defn cancelled?
  [cancel-reports]
  (>= (count (set (map :user cancel-reports))) 2))

(defn expected-arrival
  [train station delay]
  (+ (scheduled-time train station) delay))

(defn find-station
  [id]
  (first (filter #(= (:id %) id) stations)))

(defn find-train
  [number]
  (first (filter #(= (:number %) number) timetable)))

(defn reports-for
  [reports train-number type]
  (filter #(and (= (:train %) train-number) (= (:type %) type)) reports))

(defn delay-of
  [report]
  (report-delay report (find-train (:train report)) (find-station (:station report))))

(defn believable?
  [report]
  (< -3 (delay-of report) 90))

(defn estimated-delay
  [reports train-number]
  (let [delays (map delay-of (filter believable? (reports-for reports train-number :seen)))]
    (if (empty? delays) 0 (average delays))))

(defn suspicious-reports
  [reports]
  (filter (complement believable?) (filter #(= (:type %) :seen) reports)))

(defn latest-reports
  [reports n]
  (take n (reverse (sort-by :at reports))))

(defn board
  [reports station-id]
  (let [station (find-station station-id)]
    (map (fn [train]
           {:train (:number train)
            :planned (scheduled-time train station)
            :expected (expected-arrival train station (estimated-delay reports (:number train)))
            :cancelled (cancelled? (reports-for reports (:number train) :cancelled))})
         timetable)))

(defn -main
  [& args]
  (println "should be at Zemun:" (scheduled-time train zemun))
  (println "marko says it is late:" (report-delay report train zemun) "min")
  (println "board at Vukov Spomenik:" (board reports :vukov-spomenik))
  (println "ignored reports:" (map :user (suspicious-reports reports)))
  (println "latest 2 reports:" (map :user (latest-reports reports 2))))
