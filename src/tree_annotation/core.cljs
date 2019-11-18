(ns tree-annotation.core
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [markdown-to-hiccup.core :as md]
            [tree-annotation.database :as db]))


;----------------;
; Node component ;
;----------------;

(defn selection-class [node]
  "Returns a string of CSS classes to add to a node's class attribute
   based on the node's selection status."
  (cond (:selected node) " selected"
        (db/tree-selected? node) " tree-selected"
        true ""))

(defn node-component [node index]
  "Create a component (a button or text field) from a node."
  (if (:renaming node)
    [:input {:auto-focus true
             :type "text"
             :class "node"
             :value (:label node)
             :on-change #(db/rename-node (-> % .-target .-value) index)
             :on-key-down (fn [ev]
                            (when (= (.-key ev) "Enter")
                              (db/stop-renaming-node index)))}]
    [:div
     {:class (str "node" (selection-class node))
      :on-click #(db/toggle-select! index)
      :on-double-click #(db/start-renaming-node index)}
     (:label node)]))

;--------------------------------------;
; Tree component and tree manipulation ;
;--------------------------------------;

(defn tree-component [node index]
  (let [children (:children node)
        length (count children)
        component (node-component node index)]
    [:div {:class "subtree"}
     (into [:div {:class "forest children"}]
           (mapv (fn [child i] (tree-component child (conj index i)))
                 children
                 (range length)))
     component]))

(defn tree-annotation-component []
  "Creates a set of components corresponding to the nodes in the database
and some buttons for interaction."
  [:div
   [:div {:class "content"}
    [:h2 "Annotation"]
    [:div {:class "pure-button-group controls" :role "group"}
     [:button {:class "pure-button" :on-click db/combine-selected} "Combine"]
     [:button {:class "pure-button" :on-click db/deselect-all} "Deselect All"]
     [:button {:class "pure-button button-delete" :on-click db/delete-selected} "Delete"]]]
   (into
    [:div {:class "tree forest"}]
    (let [forest (db/get-forest)
          length (count forest)]
      (mapv (fn [tree i] (tree-component tree [i]))
            forest
            (range length))))])

;-----------------;
; Input component ;
;-----------------;

(defn load-input-sequence []
  "Create leaf nodes from an input string which is split on spaces."
  (let [labels (str/split (db/get-input-str) #" ")]
    (db/set-leaves labels)
    (db/toggle-io!)))

(defn sequence-input-component []
  [:div
   [:h2 "Input (list of leaves)"]
   [:div {:class "pure-form pure-g"}
    [:textarea {:class "pure-input-1"
                :value (db/get-input-str)
                :on-change #(db/set-input-str (-> % .-target .-value))
                :on-key-down (fn [ev]
                                (when (= (.-key ev) "Enter")
                                  (load-input-sequence)))}]
    [:div {:class "pure-u-1 pure-u-md-3-4"}]
    [:button {:class "pure-button pure-button-primary pure-u-1 pure-u-md-1-4"
              :on-click load-input-sequence}
     "Load Sequence"]]])

;---------------------;
; Load tree component ;
;---------------------;

(defn tree-input-component []
  [:div
   [:h2 "Input (qtree string)"]
   [:div {:class "pure-form pure-g"}
    [:textarea {:class "pure-input-1"
                :value (db/get-input-tree-str)
                :on-change #(db/set-input-tree-str (-> % .-target .-value))
                :on-key-down (fn [ev]
                               (when (= (.-key ev) "Enter")
                                 (db/load-tree-string)
                                 (db/toggle-io!)
                                 false))}]
    [:label {:class "pure-u-1 pure-u-md-1-4 pure-checkbox"}
     [:input
      {:type "checkbox"
       :checked (db/strip-math?)
       :on-change db/toggle-strip-math!}]
     " strip math"
     ]
    [:div {:class "pure-u-1 pure-u-md-1-2"}]
    [:button {:class "pure-button pure-button-primary pure-u-1 pure-u-md-1-4"
              :on-click #(do (db/load-tree-string)
                             (db/toggle-io!))}
     "Load QTree String"]]])

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
     [:div {:class "pure-form pure-g"}
      [:textarea {:value out-str
                  :class "pure-input-1"
                  :readOnly "true"}]
      [:label {:class "pure-u-1 pure-u-md-1-4 pure-checkbox"}
       [:input
        {:type "checkbox"
         :checked (db/math-inner?)
         :on-change db/toggle-math-inner!}]
       " math inner nodes"]
      [:label {:class "pure-u-1 pure-u-md-1-4 pure-checkbox"}
       [:input
        {:type "checkbox"
         :checked (db/math-leaves?)
         :on-change db/toggle-math-leaves!}]
       " math leaf nodes"]
      [:div {:class "pure-u-1 pure-u-md-1-4"}]
      [:button
       {:class "pure-button pure-u-1 pure-u-md-1-4" :on-click #(copy-to-clipboard out-str)}
       "Copy to Clipboard"]]]))

;--------------;
; IO component ;
;--------------;

(defn io-component []
  [:div
   (when (db/show-io?)
     [:div
      [sequence-input-component]
      [tree-input-component]
      [output-component]])
   [:a {:on-click db/toggle-io! :href "javascript:void(0)"} ; void() is used as a dummy href
    (if (db/show-io?) "Hide IO Section" "Show IO Section")]])


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
     [:div {:class "manual"}
      (md/md->hiccup manual-string)])
   [:a {:on-click db/toggle-manual! :href "javascript:void(0)"} ; void() is used as a dummy href
    (if (db/show-manual?) "Hide Manual" "Show Manual")]]
  )

;---------------;
; App component ;
;---------------;

(defn app-component []
  [:div
   [:div {:class "content"}
    [:h1 "Tree Annotation"]
    [manual-component]
    [io-component]]
   [tree-annotation-component]])

(defn render []
  (r/render [app-component] (js/document.getElementById "app")))

(render)

;-------------------;
; Onkeypress events ;
;-------------------;

(set! (.-onkeydown js/document)
      (fn [event]
        ;; check whether event was fired on element (e.g. text field)
        ;; or globally (target == document body)
        (when (identical? (.-target event) (.-body js/document))
          (case (.-code event)
            "Enter" (db/combine-selected)
            "Escape" (db/deselect-all)
            "Backspace" (db/delete-selected)
            "Delete" (db/delete-selected)
            nil))))
