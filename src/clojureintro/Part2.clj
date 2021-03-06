(ns clojureintro.core
  (:use bakery.core))

(defn error [& args] (apply println args) :error)

(defn loadup 
  ([n ingredient] ( if (not (nil? n)) (do (dotimes [i n] ( load-up ingredient)) :ok)))
  ([ingredient] (loadup 1 ingredient)))

(defn unload-multiple 
  ([n ingredient] (do (when (and (not= n nil) (> n 0)) (dotimes [i n] (unload ingredient))) :ok))
  ([ingredient] (unload-multiple 1 ingredient)))

(defn get-5-each-ingredients []
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

(def scooped-ingredients #{:sugar :flour :milk})
(def squeezed-ingredients #{:egg})
(def simple-ingredients #{:butter})

(defn scooped? [ingredient] (contains? scooped-ingredients ingredient))  
(defn squeezed? [ingredient] (contains? squeezed-ingredients ingredient))  
(defn simple? [ingredient] (contains? simple-ingredients ingredient))  

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
    (error "I do not have" ingredient ))
  )

(defn add  
  ([ingredient] (add 1 ingredient))
  ([n ingredient] (do (dotimes [i n] (add-ingredient ingredient)) :ok)))

(defn bake-cake-old []
  (get-5-each-ingredients)
  (add 2 :flour)
  (add 2 :egg)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies-old []
  (get-5-each-ingredients)
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

;------------Part 1 Code-----------
;----------------------------------
(defn myprintln [& args] 
  (loop [args args] 
    (if (empty? args) (println) 
      (do (print (first args) " ") (recur (rest args))))))

(def pantry-ingredients #{:sugar :flour})
(def fridge-ingredients #{:milk :egg :butter})
(def locations #{:pantry :fridge})
(def all-ingredients (apply conj pantry-ingredients fridge-ingredients))

(defn from-pantry? [ingredient]
  (contains?  pantry-ingredients ingredient))

(defn from-fridge? [ingredient]
  (contains?  fridge-ingredients ingredient))

(defn fetch ([ingredient] (fetch 1 ingredient))
  ([amount ingredient] 
   (cond 
     (from-pantry? ingredient) 
     (do 
       (go-to :pantry) 
       (loadup amount ingredient)
       (go-to :prep-area)
       (unload-multiple amount ingredient))
     (from-fridge? ingredient) 
     (do
       (go-to :fridge)
       (loadup amount ingredient)
       (go-to :prep-area)
       (unload-multiple amount ingredient))
     :else
     (error "Ingredient not in the pantry or fridge"))))

(defn fetch-from-list 
  "Fetch ingredients from a shopping-list.  The shopping-list should be a map.
  Shopping-list example: {:egg 2, :flour 3, :butter 4}"
  [shopping-list]
  (doseq [[location ingredients] {:pantry pantry-ingredients, :fridge fridge-ingredients}] 
     (go-to location) 
     (doseq [ingredient ingredients] (loadup (ingredient shopping-list 0) ingredient)))

  (go-to :prep-area)
  (doseq [[ingredient amount] shopping-list] (unload-multiple amount ingredient)))  

(def cake-ingredients {:flour 2, :egg 2, :milk 1, :sugar 1})
(def cookie-ingredients {:flour 1, :egg 1, :butter 1 , :sugar 1})

(defn bake-cake 
  "Assume cake-ingredients are already fetched"
  []
  (add 2 :flour)
  (add 2 :egg)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies 
  "Assume cookies-ingredients are already fetched"
  []
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn bake-brownies
  "Place holder for now"
  []
  )
(defn merge-maps 
  "Merges two seperate maps"
  [m1 m2] (merge-with + m1 m2))

(defn food-for-orders 
  "Return the total food for all the orders"
  [orders]
  (reduce merge-maps (map #(:items %) orders)))

(defn multiply-ingredients 
  "Multiply the amount of ingredients by n"
  [amount ingredients]
  (into {} (for [ [k v] ingredients] [k (* amount v)])))

(defn ingredients-for-orders 
  "Returns the total ingredients needed for all the orders"
  [orders]
  (let [ total-food (food-for-orders orders)
         ingredients {:cake cake-ingredients, :cookies cookie-ingredients}]
    (reduce merge-maps (for [[k v] total-food] (multiply-ingredients v (k ingredients))))))

(defn day-at-bakery 
  "Fetching ingredients one per item, baking, and then delivering that item"
  []
  (doseq [order (get-morning-orders), [item [bake-item ingredients]] {:cake [ bake-cake cake-ingredients], :cookies [bake-cookies cookie-ingredients]}] 
    (dotimes [n (item (order :items) 0)] 
      (fetch-from-list ingredients)
      (delivery {:orderid (order :orderid)
                 :address (order :address)
                 :rackids [(bake-item)]}))))

(defn day-at-bakery2 
  "Fetching all ingredients for all orders first.  Then baking each item and delivering that item"
  []
  (let [orders (get-morning-orders), all-ingredients (ingredients-for-orders orders)]
    (fetch-from-list all-ingredients)
    (doseq [order orders, [item bake-item] {:cake bake-cake, :cookies bake-cookies}]
      (dotimes [n (item (order :items) 0)]
        (delivery {:orderid (order :orderid)
                   :address (order :address)
                   :rackids [(bake-item)]})))))

(defn day-at-bakery3
  "Fetching all ingredients for all orders first.  Then baking all items per order and delivering that order of items"
  []
  (let [orders (get-morning-orders),  all-ingredients (ingredients-for-orders orders)]
    (fetch-from-list all-ingredients)
    (doseq [order orders]
      (let [bake-items {:cake bake-cake, :cookies bake-cookies, :brownies bake-brownies}, 
            all-bake-items (for [[item times] (order :items), i (range times)] ((item bake-items)))]
        (delivery {:orderid (order :orderid)
                   :address (order :address)
                   :rackids all-bake-items})))))
  

(defn -main [] (println "hello")
  (day-at-bakery3))
