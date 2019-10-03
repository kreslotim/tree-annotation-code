(ns hello-figwheel.core
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [markdown-to-hiccup.core :as md]))

;----------------------;
; Database boilerplate ;
;----------------------;

(defonce db (r/atom 
  {:input-str    "Dm G7 C"
   :output-str   ""
   :rename-label ""
   :nodes        {}
  }))


(defn get-input-str []
  (:input-str @db))

(defn set-input-str [input-str]
  (swap! db assoc :input-str input-str))


(defn get-output-str []
  (:output-str @db))

(defn set-output-str [output-str]
  (swap! db assoc :output-str output-str))


(defn get-rename-label []
  (:rename-label @db))

(defn set-rename-label [label]
  (swap! db assoc :rename-label label))

(defn get-node-coords-under-renaming []
  (->> (:nodes @db)
       (filter (fn [[coord {state :state}]] (= :rename state)))
       (map key)))


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

(defn get-node-label [coord] 
  (get-in @db [:nodes coord :label]))

(defn set-node-label [coord new-label]
  (swap! db assoc-in [:nodes coord :label] new-label))

(defn get-node-length [coord]
  (get-in @db [:nodes coord :length]))

(defn get-node-coords []
  (map key (:nodes @db)))

(defn get-selected-node-coords []
  (->> (:nodes @db)
       (filter (fn [[coord {state :state}]] (= :selected state)))
       (map key)))

(defn get-children-coords [coord]
  (get-in @db [:nodes coord :children-coords]))

(defn add-node [{:keys [x y length label children-coords state]}]
  (let [properties {:length length
                    :label label
                    :children-coords children-coords
                    :state state}]
    (swap! db assoc-in [:nodes [x y]] properties)))

(defn delete-all-nodes []
  (swap! db assoc :nodes {}))

(defn delete-node [coord]
  (swap! db (fn [db] (let [nodes (dissoc (:nodes db) coord)]
                       (assoc db :nodes nodes)))))

(defn delete-selected-nodes []
  (doall (for [coord (get-selected-node-coords)]
           (delete-node coord))))

;----------------;
; Node component ;
;----------------;

(def button-width 60)
(def button-height 20)

(defn button-style [coord]
  (let [[x y]  coord
        length (get-node-length coord)]
    {:position     "absolute"
     :left         (* x button-width)
     :top          (* y button-height)
     :width        (dec (* button-width length))
     :height       (dec button-height)
     :border-style "solid"
     :border-color "#555"
     :text-align   "center"}))

(defn node-component [coord]
  "Create a component (a button or text field) from a node."
  (let [state (get-node-state coord)]
    (if (= state :rename)
          [:input {:auto-focus true
                   :type "text" 
                   :value (get-rename-label)
                   :style (assoc (button-style coord) :z-index 1)
                   :on-change #(set-rename-label (-> % .-target .-value))}]
        (let [style (assoc (button-style coord)
                           :background-color 
                           (case state :selected     "#8E0" 
                                       :not-selected "#CCC"))]
          [:button
            {:style style :on-click #(toggle-select coord)} 
            (get-node-label coord)]))))

(defn tree-annotation-component []
  (into
    [:div {:style {:position "relative"}}]
    (map node-component (get-node-coords))))

;-----------------;
; Input component ;
;-----------------;

(defn leaf [i label]
  {:x i :y 0 :length 1 :label label :children [] :state :not-selected})

(defn reset-leafs []
  "Create leaf nodes from an input string which is split on spaces."
  (let [labels (str/split (get-input-str) #" ")
        indices (range 0 (count labels))]
    (do (delete-all-nodes))
        (doall (->> (map vector indices labels)
                    (map (partial apply leaf))
                    (map add-node)))))

(defn input-component []
  [:div
    [:button {:on-click #(reset-leafs)} "load terminals"]
    " "
    [:input {:type "text" 
             :value (get-input-str)
             :style {:width 500} 
             :on-change #(set-input-str (-> % .-target .-value))}]])

;------------------;
; Output component ;
; -----------------;

(defn tree-str [coord]
  (let [coords (get-children-coords coord)
        label  (get-node-label coord)]
    (if (empty? coords) ; if the node is a leaf
        (str "$" label "$ ")
        (apply str 
          (concat ["[.$" label "$ "] 
                  (map tree-str coords)
                  ["] "])))))

(defn compute-&-set-output-string []
  (let [coords (get-selected-node-coords)]
    (if (= 1 (count coords))
        (-> coords first tree-str set-output-str)
        (js/alert "Select exactly one node to compute its tree string."))))

(defn output-component []
  [:div
    [:button {:on-click #(compute-&-set-output-string)} "create tree string"]
    " "
    [:span {} (get-output-str)]])

;------------------;
; Manual component ;
;------------------;

(def manual-string "

# Tree Annotation Tool

## Quickstart

1. ...

## The Rules

- Only adjacent nodes can be merged.

")

(defn manual-component []
  (md/md->hiccup manual-string))

;---------------;
; App component ;
;---------------;

(defn app-component []
  [:div
   [manual-component]
   [input-component]
   [output-component]
   [tree-annotation-component]
   ])

(defn render []
  (r/render [app-component] (.-body js/document)))

(render)

;-------------------;
; Onkeypress events ;
;-------------------;

(defn create-new-node []
  (let [children-coords    (sort #(compare (get %1 0) (get %2 0)) (get-selected-node-coords))
        leftmost-coord     (first children-coords)
        rightmost-coord    (last  children-coords)
        new-node           {:x               (leftmost-coord 0)
                            :y               (inc (reduce max (map #(get % 1) children-coords)))
                            :length          (reduce + (map get-node-length children-coords))
                            :label           (get-node-label rightmost-coord)
                            :children-coords children-coords
                            :state           :selected}
        children-adjacent? (= (:length new-node)
                              (- (+ (get-node-length rightmost-coord)
                                    (rightmost-coord 0))
                                 (:x new-node)))]
    (if children-adjacent?
        (do (doall (map toggle-select children-coords))
            (add-node new-node))
        (js/alert "Only adjacent nodes can be merged."))))

(defn start-rename-node []
  (let [coords (get-selected-node-coords)]
    (if (and (= 1 (count coords)) (empty? (get-node-coords-under-renaming)))
        (let [coord (first coords)]
          (do (set-node-state coord :rename)
              (set-rename-label (get-node-label coord))))
        (js/alert "Exactly one node must be selected for renaming."))))

(set! (.-onkeypress js/document)
      (fn [event] 
        (case (.-keyCode event)
          ; enter key pressed
          13 (let [coords (get-node-coords-under-renaming)]
               (if (and (empty? coords) (seq (get-selected-node-coords)))
                   (create-new-node)
                   (let [coord (first coords)]
                     (do (set-node-label coord (get-rename-label))
                         (set-node-state coord :selected)))))
                    ;  (swap! nodes dissoc node)
                        ;  (swap! nodes assoc (assoc node :label (get-rename-label)) :selected)))))
          ; backspace key pressed
          8 (delete-selected-nodes)
          ; key 't' pressed
          ; 116 (alert-tree)
          ; key 'r' pressed
          114 (start-rename-node)
          ;(println (.-keyCode event))
          )))


                                      
; key codes
; enter <-> 13
; space <-> 32
; backspace <-> 8

; (set! (.-onkeypress js/document) (fn [event] (println (.-keyCode event))))
