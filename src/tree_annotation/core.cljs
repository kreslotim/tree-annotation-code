(ns tree-annotation.core
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [markdown-to-hiccup.core :as md]
            [tree-annotation.database :as db]))


;----------------;
; Node component ;
;----------------;

(def button-width 60)
(def button-height 20)

(defn button-style [node]
  {:position     "absolute"
   :left         (* (:x node) button-width)
   :top          (* (:y node) button-height)
   :width        (dec (* button-width (:width node)))
   :height       (dec button-height)
   :border-style "solid"
   :border-width "thin"
   :border-color "#555"
   :text-align   "center"})

(defn selection-color [node]
  ;;TODO: more colors?
  (cond (:selected node) "#8e0"
        (db/tree-selected? node) "#88e"
        true "#ccc"))

(defn node-component [node index]
  "Create a component (a button or text field) from a node."
  (if (:renaming node)
    [:input {:auto-focus true
             :type "text"
             :value (:label node)
             :style (assoc (button-style node) :z-index 1)
             :on-change #(db/rename-node (-> % .-target .-value) index)
             :on-key-press (fn [ev]
                             (when (= (.-key ev) "Enter")
                               (db/stop-renaming-node index)))}]
    (let [style (assoc (button-style node)
                       :background-color (selection-color node)
                       :cursor "pointer"
                       )]
      [:div
       {:style style
        :type "button"
        :on-click #(db/toggle-select! index)
        :on-double-click #(db/start-renaming-node index)}
       (:label node)])))

;--------------------------------------;
; Tree component and tree manipulation ;
;--------------------------------------;

;; todo: make component structure nested
(defn tree-components [node index]
  (let [children (:children node)
        length (count children)
        component (node-component node index)]
    (into [component]
          (reduce concat (map (fn [child i] (tree-components child (conj index i)))
                              children
                              (range length))))))

(defn tree-annotation-component []
  "Creates a set of components corresponding to the nodes in the database
and some buttons for interaction."
  [:div
   [:h2 "Annotation"]
   [:div
    [:button {:on-click db/combine-selected} "Combine"]
    [:button {:on-click db/delete-selected} "Delete"]
    [:button {:on-click db/deselect-all} "Deselect All"]]
   [:br]
   (into
    [:div {:style {:position "relative"}}]
    (let [forest (db/get-forest)
          length (count forest)]
      (flatten1 (map (fn [tree i] (tree-components tree [i]))
                     forest
                     (range length)))))])

;-----------------;
; Input component ;
;-----------------;

(defn load-input-sequence []
  "Create leaf nodes from an input string which is split on spaces."
  (let [labels (str/split (db/get-input-str) #" ")]
    (db/set-leaves labels)))

(defn sequence-input-component []
  [:div
   [:h2 "Input (list of leaves)"]
   [:textarea {:value (db/get-input-str)
               :size (+ (count (db/get-input-str)) 2)
               :style {:width 520}
               :on-change #(db/set-input-str (-> % .-target .-value))}]
   [:br]
   [:button {:on-click load-input-sequence} "Load Sequence"]])

;---------------------;
; Load tree component ;
;---------------------;

(defn tree-input-component []
  [:div
   [:h2 "Input (qtree string)"]
   [:textarea {:value (db/get-input-tree-str)
               :size (+ (count (db/get-input-tree-str)) 2)
               :style {:width 520}
               :on-change #(db/set-input-tree-str (-> % .-target .-value))}]
   [:div
    [:label
     [:input
      {:type "checkbox"
       :checked (db/strip-math?)
       :on-change db/toggle-strip-math!}]
     "strip math"
     ]
    [:button {:on-click db/load-tree-string} "Load QTree String"]]])

;------------------;
; Output component ;
; -----------------;

(defn copy-to-clipboard [str]
  (let [el (js/document.createElement "textarea")]
    (set! (.-value el) str)
    (.appendChild js/document.body el)
    (.select el)
    (.setSelectionRange el 0 99999)
    (js/document.execCommand "copy")
    (.removeChild js/document.body el)))

(defn output-component []
  (let [out-str (db/get-output-str)]
    [:div
     [:h2 "Output (qtree string)"]
     [:textarea {:value out-str
                 :style {:width 520}
                 :readonly "readonly"}]
     [:div
      [:label
       [:input
        {:type "checkbox"
         :checked (db/math-inner?)
         :on-change db/toggle-math-inner!}]
       "$ inner nodes"]
      [:label
       [:input
        {:type "checkbox"
         :checked (db/math-leaves?)
         :on-change db/toggle-math-leaves!}]
       "$ leaf nodes"]
      [:button
       {:on-click #(copy-to-clipboard out-str)}
       "Copy to Clipboard"]]]))

;------------------;
; Manual component ;
;------------------;

(def manual-string "

## Manual

by [Daniel Harasim](https://dcml.epfl.ch/lab/harasim/),
[Christoph Finkensiep](https://dcml.epfl.ch/lab/finkensiep/),
and the [Digital and Cognitive Musicology Lab (DCML)](https://dcml.epfl.ch)

This is an open source project. Find the code [here](https://github.com/DCMLab/tree-annotation-code).

### Quickstart

1. Write the sequence that you want to annotate with a tree into the text field.
   The elements of the sequence must be separated by space characters.
1. Press the *load sequence* button to load the sequence as leaf nodes of a tree.
1. Select nodes that you want to combine by clicking on them.
   Clicking again deselects a selected node.
   If the node is not a root, the path to the root will be highlighted too, in a different color.
1. Press `Enter` (or click on `Combine`) to combine the selected subtrees into a new tree.
   Only adjacent subtrees can be combined.
   If there are several adjacent groups of trees selected,
   each group will be combined into a new tree.
1. Combine as many more nodes as you like to create the complete tree.
1. The current structure of the tree will be shown in the output field
   as a string usable by the LaTeX package
   [tikz-qtree](http://www.pirbot.com/mirrors/ctan/graphics/pgf/contrib/tikz-qtree/tikz-qtree-manual.pdf).
   The string can also copied to the clipboard by pressing the `Copy` button.
1. Render the tree in a latex document using the *tikz-qtree* package.

###  Additional Functionality

- Double clicking on a node opens a text field to rename that node.
  Submit the new name by pressing `Enter`.
- Pressing `Delete` or `Backspace` (or clicking the `Delete` button)
  deletes all selected nodes and their ancestors.
  Only inner nodes or the last leaf node can be deleted.
- Pressing `Esc` (or clicking the `Deselect All` button) deselects all nodes.
- You can also edit an existing qtree string by loading it 
  using the *load qtree string* button.

")

(defn manual-component []
  [:div
   (when (db/show-manual?)
     [:div {:style {:max-width 600 :border-style "solid" :padding 20}}
      (md/md->hiccup manual-string)])
   [:button {:on-click db/toggle-manual}
    (if (db/show-manual?) "Hide Manual" "Show Manual")]]
  )

;---------------;
; App component ;
;---------------;

(defn app-component []
  [:div {:style {:font-family "Helvetica Neue"}}
   [:h1 "Tree Annotation"]
   [manual-component]
   [sequence-input-component]
   [tree-input-component]
   [output-component]
   [tree-annotation-component]])

(defn render []
  (r/render [app-component] (.-body js/document)))

(render)

;-------------------;
; Onkeypress events ;
;-------------------;

(set! (.-onkeydown js/document)
      (fn [event]
        (case (.-code event)
          "Enter" (db/combine-selected)
          "Escape" (db/deselect-all)
          "Backspace" (db/delete-selected)
          "Delete" (db/delete-selected))))
