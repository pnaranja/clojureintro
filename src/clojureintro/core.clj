(ns clojureintro.core
  (:use bakery.core))

(defn loadup 
  ([n ingredient] (do (dotimes [i n] ( load-up ingredient)) :ok))
  ([ingredient] (loadup 1 ingredient)))

(defn unload-multiple 
  ([n ingredient] (do (dotimes [i n] (unload ingredient)) :ok))
  ([ingredient] (unload-multiple 1 ingredient)))

(defn get-ingredients []
  (start-over)
  (go-to :fridge)
  (loadup 5 :egg)
  (loadup 5 :butter)
  (loadup 5 :milk)
  (go-to :pantry)
  (loadup 5 :flour)
  (loadup 5 :sugar)
  (go-to :prep-area)
  (unload-multiple 5 :egg)
  (unload-multiple 5 :butter)
  (unload-multiple 5 :flour)
  (unload-multiple 5 :milk)
  (unload-multiple 5 :sugar))

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
  (get-ingredients)
  (add 2 :flour)
  (add 2 :egg)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (get-ingredients)
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn -main [] (println "hello")
  (bake-cake)
  (bake-cookies)
  )
