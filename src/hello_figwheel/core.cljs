(ns hello-figwheel.core
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [markdown-to-hiccup.core :as md]))

(def manual-string "

# Tree Annotation Tool

## Quickstart

1. ...

## The Rules

- Only adjacent nodes can be merged.

")

(def initial-input-string "Dm G7 C")

(def button-width 60)
(def button-height 20)

(defn x-coord [x] (* x button-width))
(defn y-coord [x] (* x button-height))

(defn button-style [{:keys [x y length]}]
  {:position     "absolute" 
   :left         (x-coord x)
   :top          (y-coord y)
   :width        (dec (* button-width length))
   :height       (dec button-height)
   :border-style "solid"
   :border-color "#555"
   :text-align   "center"})

(defn node [x y length label children]
  "Create a tree node that will be rendered as a button."
  {:x        x
   :y        y
   :length   length
   :label    label
   :children children})

(defn leaf [i label]
  "Leafs are terminal nodes"
  (node i 0 1 label []))

(defn leafs [input-string]
  "Create leaf nodes from an input string which will be split on spaces."
  (let [labels (str/split input-string #" ")
        indices (range 0 (count labels))]
    (map (partial apply leaf)
         (map vector indices labels))))

(defonce nodes (r/atom {}))
  ; (apply merge (map #(hash-map % :not-selected) (leafs initial-input-string)))))

(defn toggle-select [node]
  "Select a node if it is not selected or 
   unselect it if it is selected."
  (let [new-state (case (@nodes node)
                     :selected :not-selected
                     :not-selected :selected)]
    (swap! nodes assoc node new-state)))

(defonce rename-buffer (r/atom ""))

(defn button<-node [node]
  "Create a react component (a button) from a node."
  (let [status (@nodes node)]
    (if (= status :rename)
          [:input {:auto-focus true
                   :type "text" 
                   :value @rename-buffer
                   :style (assoc (button-style node) :z-index 1) ; (assoc :width (- (:width node) 100)))
                   :on-change #(reset! rename-buffer (-> % .-target .-value))}]
                  ;  :onkeydown (fn [event] (when (= 27 (.-keyCode event)) ; Esc key pressed
                                ;  (js/alert "hi") 
                                ;  (swap! nodes dissoc node)
                                ;  (swap! nodes assoc (assoc node :label @rename-buffer) :selected) ))}]
        (let [style (-> (button-style node)
                        (assoc :background-color 
                          (case status :selected "#8E0" :not-selected "#CCC")))]
          [:button
            {:style style :on-click (fn [] (toggle-select node))} 
            (:label node)]))))

(defmulti tree-str (comp count :children))
(defmethod tree-str 0 [{label :label}]
  (str "$" label "$ "))
(defmethod tree-str 1 [{label :label, [child] :children}]
  (str "[.$" label "$ " (tree-str child) "] "))
(defmethod tree-str 2 [{label :label, [child1 child2] :children}]
  (str "[.$" label "$ " (tree-str child1) " " (tree-str child2) "] "))

(defn output-treestring [treestr-atom]
  (reset! treestr-atom "ho"))

(defn output-component []
  (let [treestr-atom (r/atom "")]
    (fn []
      [:div
        [:button {:on-click #(output-treestring treestr-atom)} "create treestring"]
        " "
        [:span {} @treestr-atom]])))

(defn manual-component []
  (md/md->hiccup manual-string))

(defn tree-annotation-component []
  (into
    [:div {:style {:position "relative"}}]
    (map button<-node (keys @nodes))))

(defonce input-string (r/atom initial-input-string))

(defn load-terminals []
  (reset! nodes (apply merge (map #(hash-map % :not-selected) 
                                   (leafs @input-string)))))

(defn leaf-input-component []
  [:div
    [:button {:on-click #(load-terminals)} "load terminals"]
    " "
    [:input {:type "text" 
             :value @input-string
             :style {:width 500} 
             :on-change #(reset! input-string (-> % .-target .-value))}]])

(defn app-component []
  [:div
   [manual-component]
   [leaf-input-component]
   [output-component]
   [tree-annotation-component]])

(defn nodes-with-status [s]
  (->> @nodes
       (filter #(= s (val %)))
       (map key)
       (sort #(compare (:x %1) (:x %2)))))

(defn selected-nodes []
  (nodes-with-status :selected))

(defn create-new-node []
  (let [children        (selected-nodes)
        leftmost-child  (first children)
        rightmost-child (last  children)
        x               (:x leftmost-child)
        y               (inc (reduce max (map :y children)))
        length          (reduce + (map :length children))
        label           (:label rightmost-child)
        new-node        (node x y length label children)]
    (if (= length (- (+ (:length rightmost-child) (:x rightmost-child)) x))
        (do
          (doall (map #(swap! nodes assoc % :not-selected) children))
          (swap! nodes assoc new-node :selected))
        (js/alert "Only adjacent nodes can be merged."))))

(defn delete-nodes []
  (doall (map #(swap! nodes dissoc (key %)) (filter #(= :selected (val %)) @nodes))))

(defn alert-tree []
  (let [s-nodes (selected-nodes)]
    (if (= 1 (count s-nodes))
        (js/alert (tree-str (first s-nodes)))
        (js/alert "Only a single node must be selected to compute its tree string."))))

(defn rename-node []
  (let [s-nodes (selected-nodes)]
    (if (and (= 1 (count s-nodes)) (empty? (nodes-with-status :rename)))
        (let [node (first s-nodes)]
          (do (swap! nodes assoc node :rename)
              (reset! rename-buffer (:label node))))
        (js/alert "Exactly one node must be selected for renaming."))))

(defn render []
  (r/render [app-component] (.-body js/document)))

(set! (.-onkeypress js/document)
      (fn [event] 
        (case (.-keyCode event)
          ; enter key pressed
          13 (let [r-nodes (nodes-with-status :rename)]
               (if (and (empty? r-nodes) (seq (selected-nodes)))
                   (create-new-node)
                   (let [node (first r-nodes)]
                     (do (swap! nodes dissoc node)
                         (swap! nodes assoc (assoc node :label @rename-buffer) :selected)))))
          ; backspace key pressed
          8 (delete-nodes)
          ; key 't' pressed
          116 (alert-tree)
          ; key 'r' pressed
          114 (rename-node)
          ;(println (.-keyCode event))
          )))


                                      
; key codes
; enter <-> 13
; space <-> 32
; backspace <-> 8

; (set! (.-onkeypress js/document) (fn [event] (println (.-keyCode event))))

(render)