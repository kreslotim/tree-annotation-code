;-------------------------;
; Database state managing ;
;-------------------------;

(ns tree-annotation.database
  (:require [reagent.core :as r]))

; private annotation does currently not work in clojure script
(defonce ^:private db (r/atom 
  {:input-str    "Dm G7 C"
   :output-str   ""
   :rename-label ""
   :nodes        {}
  }))

;-----------------------;
; Input string requests ;
;-----------------------;

(defn ^:private get-input-str []
  (:input-str @db))

(defn set-input-str [input-str]
  (swap! db assoc :input-str input-str))

;------------------------;
; Output string requests ;
;------------------------;

(defn get-output-str []
  (:output-str @db))

(defn set-output-str [output-str]
  (swap! db assoc :output-str output-str))

;-----------------------;
; Rename label requests ;
;-----------------------;

(defn get-rename-label []
  (:rename-label @db))

(defn set-rename-label [label]
  (swap! db assoc :rename-label label))

(defn get-node-coords-under-renaming []
  (->> (:nodes @db)
       (filter (fn [[coord {state :state}]] (= :rename state)))
       (map key)))

;---------------------;
; Node state requests ;
;---------------------;

(defn get-node-state [coord] 
  (get-in @db [:nodes coord :state]))

(defn set-node-state [coord new-state]
  (swap! db assoc-in [:nodes coord :state] new-state))

(defn toggle-select [coord]
  "Select a node if it is not selected or 
   unselect it if it is selected."
  (let [new-state (case (get-node-state coord)
                     :selected :not-selected
                     :not-selected :selected)]
    (set-node-state coord new-state)))

;---------------------;
; Node label requests ;
;---------------------;

(defn get-node-label [coord] 
  (get-in @db [:nodes coord :label]))

(defn set-node-label [coord new-label]
  (swap! db assoc-in [:nodes coord :label] new-label))

;----------------------;
; Node length requests ;
;----------------------;

(defn get-node-length [coord]
  (get-in @db [:nodes coord :length]))

;--------------------------;
; Node coordinate requests ;
;--------------------------;

; IMPORTANT: Coordinates are not represented as maps,
; but as vectors [x y] where x and y are integers.

(defn get-node-coords []
  (map key (:nodes @db)))

(defn get-selected-node-coords []
  (->> (:nodes @db)
       (filter (fn [[coord {state :state}]] (= :selected state)))
       (map key)))

;------------------------;
; Node children requests ;
;------------------------;

(defn get-children-coords [coord]
  (get-in @db [:nodes coord :children-coords]))

(defn leaf? [coord]
  (empty? (get-children-coords coord)))

(defn last-leaf? [coord]
  (and (leaf? coord)
       (= (first coord)
          (->> (get-node-coords)
               (map first)
               (apply max)))))

(defn last-leaf-or-inner-node? [coord]
  (or (last-leaf? coord)
      (not (leaf? coord))))

;----------------------;
; Node parent requests ;
;----------------------;

(defn get-parent-coord [coord]
  (get-in @db [:nodes coord :parent-coord]))

(defn set-parent-coord [coord parent-coord]
  (swap! db assoc-in [:nodes coord :parent-coord] parent-coord))

(defn del-parent-coord [coord]
  (swap! db
    (fn [db]
      (assoc-in db [:nodes coord]
        (dissoc (get-in db [:nodes coord]) :parent-coord)))))

(declare del-node)

(defn del-ancestors [coord]
  (let [parent-coord (get-parent-coord coord)]
    (when (some? parent-coord)
      (do (del-ancestors parent-coord)
          (del-node parent-coord)
          (del-parent-coord coord)))))

;------------------------------;
; Node add and delete requests ;
;------------------------------;

(defn add-node [{:keys [x y length label children-coords state]}]
  (let [properties {:length length
                    :label label
                    :children-coords children-coords
                    :state state}]
    (do (swap! db assoc-in [:nodes [x y]] properties)
        (doall (map #(set-parent-coord % [x y]) children-coords)))))

(defn del-all-nodes []
  (swap! db assoc :nodes {}))

(defn del-node [coord]
  (when (last-leaf-or-inner-node? coord)
    (do (del-ancestors coord)
        (swap! db 
          (fn [db]
            (assoc db :nodes 
              (dissoc (:nodes db) coord)))))))

(defn del-selected-nodes []
  (doall (for [coord (get-selected-node-coords)]
           (del-node coord))))