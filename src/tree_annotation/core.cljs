(ns tree-annotation.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [markdown-to-hiccup.core :as md]
            [tree-annotation.database :as db]))


;----------------;
; Node component ;
;----------------;

(defn selection-class [node]
  "Returns a string of CSS helper classes to add to a node's class attribute
   indicating the node's selection status."
  (cond (:selected node) "selected"
        (db/tree-selected? node) "tree-selected"
        true ""))

(defn node-component [node index]
  "Create a component (a button or text field) from a node."
  (if (:renaming node)
    [:input.node
     {:auto-focus true
      :type "text"
      :value (:label node)
      :on-change #(db/rename-node (-> % .-target .-value) index)
      :on-focus #(-> % .-target .select)
      :on-key-down (fn [ev]
                     (when (= (.-key ev) "Enter")
                       (db/stop-renaming-node index)))}]
    [:div.node
     {:class (selection-class node)
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
    [:div.subtree
     (into [:div.forest.children]
           (mapv (fn [child i] (tree-component child (conj index i)))
                 children
                 (range length)))
     component]))

(defn tree-annotation-component []
  "Creates a set of components corresponding to the nodes in the database
and some buttons for interaction."
  [:div
   [:div.content
    [:h2 "Annotation"]
    [:div.pure-button-group.controls
     {:role "group"}
     [:button.pure-button
      {:on-click db/combine-selected} "Combine"]
     [:button.pure-button
      {:on-click db/deselect-all} "Deselect All"]
     [:button.pure-button.button-delete
      {:on-click db/delete-selected} "Delete"]]]
   (into
    [:div.tree.forest]
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
    (db/toggle-input!)))

(defn sequence-input-component []
  [:div
   [:h3 "List of Leaves"]
   [:div.pure-form.pure-g
    [:textarea.pure-input-1
     {:value (db/get-input-str)
      :on-change #(db/set-input-str (-> % .-target .-value))
      :on-key-down (fn [ev]
                     (when (= (.-key ev) "Enter")
                       (load-input-sequence)))}]
    [:div.pure-u-1.pure-u-md-3-4]
    [:button.pure-button.pure-button-primary.pure-u-1.pure-u-md-1-4
     {:on-click load-input-sequence}
     "Load Sequence"]]])

(defn tree-input-component []
  [:div
   [:h3 "qtree String"]
   [:div.pure-form.pure-g
    [:textarea.pure-input-1
     {:value (db/get-input-qtree-str)
      :on-change #(db/set-input-qtree-str (-> % .-target .-value))
      :on-key-down (fn [ev]
                     (when (= (.-key ev) "Enter")
                       (db/load-qtree-string)
                       (db/toggle-input!)
                       false))}]
    [:label.pure-u-1.pure-u-md-1-4.pure-checkbox
     [:input
      {:type "checkbox"
       :checked (db/strip-math?)
       :on-change db/toggle-strip-math!}]
     " strip math"
     ]
    [:div.pure-u-1.pure-u-md-1-2]
    [:button.pure-button.pure-button-primary.pure-u-1.pure-u-md-1-4
     {:on-click #(do (db/load-qtree-string)
                     (db/toggle-input!))}
     "Load QTree String"]]
   [:h3 "JSON"]
   [:div.pure-form.pure-g
    [:textarea.pure-input-1
     {:value (db/get-input-json-str)
      :on-change #(db/set-input-json-str (-> % .-target .-value))
      :on-key-down (fn [ev]
                     (when (= (.-key ev) "Enter")
                       (db/load-json-string)
                       (db/toggle-input!)
                       false))}]
    [:div.pure-u-1.pure-u-md-3-4]
    [:button.pure-button.pure-button-primary.pure-u-1.pure-u-md-1-4
     {:on-click #(do (db/load-json-string)
                     (db/toggle-input!))}
     "Load JSON String"]]
   ])

(defn input-component []
  [:div
   (when (db/show-input?)
     [:div
      [:h2 "Input"]
      [sequence-input-component]
      [tree-input-component]
      ])
   [:a {:on-click db/toggle-input! :href "javascript:void(0)"} ; void() is used as a dummy href
       (if (db/show-input?) "Hide Input" "Show Input")]])

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

(defn qtree-output-component []
  (let [out-str-qtree (db/get-output-str-qtree)]
    [:div.pure-form.pure-g
     [:h3.pure-u-1 "qtree String"]
      [:label.pure-u-1.pure-u-md-1-4.pure-checkbox
       [:input
        {:type "checkbox"
         :checked (db/math-inner?)
         :on-change db/toggle-math-inner!}]
       " math inner nodes"]
      [:label.pure-u-1.pure-u-md-1-4.pure-checkbox
       [:input
        {:type "checkbox"
         :checked (db/math-leaves?)
         :on-change db/toggle-math-leaves!}]
       " math leaf nodes"]
      [:div.pure-u-1.pure-u-md-1-4]
      [:textarea.pure-input-1.output
       {:value out-str-qtree
        :readOnly "true"}]
      [:button.pure-button.pure-u-1.pure-u-md-1-4
       {:on-click #(copy-to-clipboard out-str-qtree)}
       "Copy to Clipboard"]]))

(defn json-output-component []
  (let [out-str-json (db/get-output-str-json)]
    [:div.pure-form.pure-g
     [:h3.pure-u-1 "JSON String"]
     [:label.pure-u-1.pure-u-md-1-4.pure-checkbox
      [:input
       {:type "checkbox"
        :checked (db/pretty-print-json?)
        :on-change db/toggle-pretty-print-json!}]
      " pretty print"]
     [:textarea.pure-input-1.output
      {:value out-str-json
       :readOnly "true"}]
     [:button.pure-button.pure-u-1.pure-u-md-1-4
      {:on-click #(copy-to-clipboard out-str-json)}
      "Copy to Clipboard"]]))

(defn output-component []
  [:div
   (when (db/show-output?)
     [:div
      [:h2 "Output"]
      (when (not= (count (db/get-forest)) 1)
        [:div.alert "Warning: tree is incomplete!"])
      [qtree-output-component]
      [json-output-component]])
   [:a {:on-click db/toggle-output! :href "javascript:void(0)"} ; void() is used as a dummy href
       (if (db/show-output?) "Hide Output" "Show Output")]])

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
  Pressing `e` or `r` opens a text field for every selected node.
- Pressing `Delete` or `Backspace` (or clicking the `Delete` button)
  deletes all selected nodes and their ancestors.
  Only inner nodes or the last leaf node can be deleted.
- Pressing `Esc` (or clicking the `Deselect All` button) deselects all nodes.
- Pressing `i` or `o` toggles the input or output section, respectively.
  Pressing `m` or `?` toggles the manual section.
- You can also edit an existing qtree string by loading it 
  using the *load qtree string* button.

")

(defn manual-component []
  [:div
   (when (db/show-manual?)
     [:div.manual
      (md/md->hiccup manual-string)])
   [:a {:on-click db/toggle-manual! :href "javascript:void(0)"} ; void() is used as a dummy href
    (if (db/show-manual?) "Hide Manual" "Show Manual")]])

;---------------;
; App component ;
;---------------;

(defn app-component []
  [:div
   [:div.content
    [:h1 "Tree Annotation"]
    [manual-component]
    [input-component]]
   [tree-annotation-component]
   [:div.content
    [output-component]]
   [:div.bottom-whitespace]])

(defn render []
  (rdom/render [app-component] (js/document.getElementById "app")))

(render)

;-------------------;
; Onkeypress events ;
;-------------------;

(set! (.-onkeydown js/document)
      (fn [event]
        ;; check whether event was fired on element (e.g. text field)
        ;; or globally (target == document body)
        (when (identical? (.-target event) (.-body js/document))
          (case (.-key event)
            "Enter" (db/combine-selected)
            "Escape" (db/deselect-all)
            "Backspace" (db/delete-selected)
            "Delete" (db/delete-selected)
            "i" (db/toggle-input!)
            "o" (db/toggle-input!)
            "?" (db/toggle-manual!)
            "m" (db/toggle-manual!)
            "e" (db/start-renaming-selected)
            "r" (db/start-renaming-selected)
            nil)
          (.preventDefault event)
          (.stopPropagation event)
          false)))
