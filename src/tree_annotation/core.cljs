(ns tree-annotation.core
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [markdown-to-hiccup.core :as md]
            [instaparse.core :as insta :refer-macros [defparser]]
            [tree-annotation.database :as db]))


;----------------;
; Node component ;
;----------------;

(def button-width 60)
(def button-height 20)

(defn button-style [coord]
  (let [[x y]  coord
        length (db/get-node-length coord)]
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
  (let [state (db/get-node-state coord)]
    (if (= state :rename)
          [:input {:auto-focus true
                   :type "text" 
                   :value (db/get-rename-label)
                   :style (assoc (button-style coord) :z-index 1)
                   :on-change #(db/set-rename-label (-> % .-target .-value))}]
        (let [style (assoc (button-style coord)
                           :background-color 
                           (case state :selected     "#8E0" 
                                       :not-selected "#CCC"))]
          [:button
            {:style style :on-click #(db/toggle-select coord)} 
            (db/get-node-label coord)]))))

(defn tree-annotation-component []
  (into
    [:div {:style {:position "relative"}}]
    (map node-component (db/get-node-coords))))

;-----------------;
; Input component ;
;-----------------;

(defn leaf [i label]
  {:x i :y 0 :length 1 :label label :children [] :state :not-selected})

(defn reset-leafs []
  "Create leaf nodes from an input string which is split on spaces."
  (let [labels (str/split (db/get-input-str) #" ")
        indices (range 0 (count labels))]
    (do (db/del-all-nodes))
        (doall (->> (map vector indices labels)
                    (map (partial apply leaf))
                    (map db/add-node)))))

(defn input-component []
  [:div
    {:style {:position "relative"}}
    [:button {:on-click #(reset-leafs)} "load sequence"]
    [:input {:type "text" 
             :value (db/get-input-str)
             :size (+ (count (db/get-input-str)) 2) 
             :style {:position "absolute" :left 120}
             :on-change #(db/set-input-str (-> % .-target .-value))}]])

;---------------------;
; Load tree component ;
;---------------------;

(defparser tree-parser "
  node = leaf | <'[.'> label <' '> node+ <']'> ' '?
  leaf = label <' '>
  label = #'[^\\[\\]. ]+'
  ")

;------------------;
; Output component ;
; -----------------;

(defn tree-str [coord]
  (let [coords (db/get-children-coords coord)
        label  (db/get-node-label coord)]
    (if (empty? coords) ; if the node is a leaf
        (str "$" label "$ ")
        (apply str 
          (concat ["[.$" label "$ "] 
                  (map tree-str coords)
                  ["] "])))))

(defn copy-to-clipboard [str]
  (let [el (js/document.createElement "textarea")]
    (set! (.-value el) str)
    (.appendChild js/document.body el)
    (.select el)
    (.setSelectionRange el 0 99999)
    (js/document.execCommand "copy")
    (.removeChild js/document.body el)))
 
(defn compute-&-set-output-string []
  (let [coords (db/get-selected-node-coords)]
    (if (= 1 (count coords))
        (do (-> coords first tree-str db/set-output-str)
            (copy-to-clipboard (db/get-output-str)))
        (js/alert "Select exactly one node to compute its tree string."))))

(defn output-component []
  [:div
    {:style {:position "relative"}}
    [:button 
      {:on-click #(compute-&-set-output-string)} 
      "create tree string"]
    [:span 
      {:style {:position "absolute" :left 120 :font-size 15}} 
      (db/get-output-str)]])

;------------------;
; Manual component ;
;------------------;

(def manual-string "

# Tree Annotation Tool

by [Daniel Harasim](https://dcml.epfl.ch/lab/harasim/) 
and the [Digital and Cognitive Musicology Lab (DCML)](https://dcml.epfl.ch)

This is an open source project. Find the code [here](https://github.com/DCMLab/tree-annotation-code).

## Quickstart

1. Write the sequence that you want to annotate with a tree into the text field.
   The elements of the sequence must be separated by space characters.
1. Press the *load sequence* button to load the sequence as leaf nodes of a tree.
1. Select nodes that you want to combine using the mouse.
1. Press `Enter` to combine the selcted nodes into a subtree. 
   (Only adjacent nodes that do not overlap can be combined.)
1. Combine as many more nodes as you like to create the tree.
1. Select the root node of the tree and press the button *create tree string* 
   to display the tree as a string used by the latex package 
   [tikz-qtree](http://www.pirbot.com/mirrors/ctan/graphics/pgf/contrib/tikz-qtree/tikz-qtree-manual.pdf).
   The string is also copied to the clipboard.
1. Render the tree in a latex document using the *tikz-qtree* package.

##  Additional Functionality

- `Comma` opens a text field to rename a selected node. 
  Submit the new name by pressing `Enter`.
- `Backspace` deletes all selected nodes and their ancestors.
  Only inner nodes or the last leaf node can be deleted.
- `Esc` deselects all nodes.

")

(defn manual-component []
  [:div
    {:style {:max-width 600 :border-style "solid" :padding 20}}
    (md/md->hiccup manual-string)])

;---------------;
; App component ;
;---------------;

(defn app-component []
  [:div {:style {:font-family "Helvetica Neue"}}
    [manual-component]
    [:div {:style {:height 30}}]
    [:div [input-component] ]
    [:br]
    [:div [output-component] ]
    [:br]
    [tree-annotation-component]])

(defn render []
  (r/render [app-component] (.-body js/document)))

(render)

;-------------------;
; Onkeypress events ;
;-------------------;

(defn create-new-node []
  (let [children-coords    (sort #(compare (get %1 0) (get %2 0)) 
                                 (db/get-selected-node-coords))
        leftmost-coord     (first children-coords)
        rightmost-coord    (last  children-coords)
        new-node           
          {:x              (leftmost-coord 0)
          :y               (inc (reduce max (map #(get % 1) children-coords)))
          :length          (reduce + (map db/get-node-length children-coords))
          :label           (db/get-node-label rightmost-coord)
          :children-coords children-coords
          :state           :selected}
        children-adjacent? (= (:length new-node)
                              (- (+ (db/get-node-length rightmost-coord)
                                    (rightmost-coord 0))
                                 (:x new-node)))]
    (if children-adjacent?
        (do (doall (map db/toggle-select children-coords))
            (db/add-node new-node))
        (js/alert "Only adjacent nodes can be merged."))))

(defn start-rename-node []
  (let [coords (db/get-selected-node-coords)]
    (if (and (= 1 (count coords)) (empty? (db/get-node-coords-under-renaming)))
        (let [coord (first coords)]
          (do (db/set-node-state coord :rename)
              (db/set-rename-label (db/get-node-label coord))))
        (js/alert "Exactly one node must be selected for renaming."))))

(defn deselect-all-nodes []
  (doall 
    (for [coord (db/get-node-coords)]
      (db/set-node-state coord :not-selected))))

(set! (.-onkeypress js/document)
      (fn [event] 
        (case (.-code event)
          ; enter key pressed
          "Enter" (let [coords (db/get-node-coords-under-renaming)]
               (if (and (empty? coords) (seq (db/get-selected-node-coords)))
                   (create-new-node)
                   (let [coord (first coords)]
                     (do (db/set-node-label coord (db/get-rename-label))
                         (db/set-node-state coord :selected)))))
                    ;  (swap! nodes dissoc node)
                        ;  (swap! nodes assoc (assoc node :label (get-rename-label)) :selected)))))
          ; backspace key pressed
          "Backspace" (db/del-selected-nodes)
          ; tab key pressed
          "Comma" (start-rename-node)
          ; esc key pressed
          "Escape" (deselect-all-nodes)
          )))
