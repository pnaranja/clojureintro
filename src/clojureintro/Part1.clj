(ns clojureintro.core)

(load-file "src/clojureintro/video1.clj")

(println "\nBaking a Cake\n")
(defn add2 [ingredient]
  (cond 
    (= ingredient :egg) (add-egg)
    (= ingredient :sugar) (add-sugar)
    (= ingredient :flour) (add-flour)
    (= ingredient :milk) (add-milk)
    (= ingredient :butter) (add-butter)
    :else (do (println "Unknown ingredient:" ingredient) :error)))

(start-over)
(add2 :flour)
(add2 :flour)
(add2 :egg)
(add2 :egg)
(add2 :milk)
(add2 :sugar)
(release)
(mix)
(pour-into-pan)
(bake-pan 25)
(cool-pan)
(status)

(defn scooped? [ingredient]
  ((complement not-any?) #(= ingredient %) (list :sugar :flour :milk )))

(defn squeezed? [ingredient] (= ingredient :egg))
(defn simple? [ingredient] (= ingredient :butter))

(defn cannot-action [ingredient action] (println "Sorry, cannot" action ingredient))

(defn add-scooped [ingredient] (if (scooped? ingredient) 
                                 (do (grab :cup) (scoop ingredient) (add-to-bowl) (release)) 
                                 (do (cannot-action ingredient "scoop") :error)))

(defn add-sequeezed [ingredient] (if (squeezed? ingredient) 
                                  (do (grab ingredient) (squeeze) (add-to-bowl)) 
                                  (do (cannot-action ingredient "squeeze") :error)))

(defn add-simple [ingredient] (if (simple? ingredient) 
                                (do (grab ingredient) (add-to-bowl)) 
                                (cannot-action ingredient "add")))

(defn add-ingredient [ingredient]
  (cond
    (scooped? ingredient) (add-scooped ingredient)
    (squeezed? ingredient) (add-sequeezed ingredient)
    (simple? ingredient) (add-simple ingredient)
    :else
    (do (println "I do not have" ingredient) :error))
  )

(defn add  
  ([ingredient] (add 1 ingredient))
  ([n ingredient] (do (dotimes [i n] (add-ingredient ingredient)) :ok)))

(defn bake-cake []
  (start-over)
  (add 2 :flour)
  (add 2 :egg)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (start-over)
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))
