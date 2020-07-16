;-------------------------;
; Database state managing ;
;-------------------------;

(ns tree-annotation.database
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [instaparse.core :as insta :refer-macros [defparser]]))

; private annotation does currently not work in clojure script
(defonce ^:private db (r/atom 
  {:input-str         "Dm G7 C"
   :input-tree-str    "[.$C$ [.$G7$ $Dm$ $G7$ ] $C$ ] "
   :math-inner        true
   :math-leaves       true
   :pretty-print-json false
   :strip-math        true
   :forest            []
   :show-manual       true
   :show-io           true
  }))

;-----------------------;
; Input string requests ;
;-----------------------;

(defn get-input-str []
  (:input-str @db))

(defn set-input-str [input-str]
  (swap! db assoc :input-str (str/trim-newline input-str)))

;----------------------------;
; Input tree string requests ;
;----------------------------;

(defn get-input-tree-str []
  (:input-tree-str @db))

(defn set-input-tree-str [input-tree-str]
  (swap! db assoc :input-tree-str (str/trim-newline input-tree-str)))

(defn strip-math? []
  (:strip-math @db))

(defn toggle-strip-math! []
  (swap! db update :strip-math not))

;------------------------;
; Output string requests ;
;------------------------;

(declare leaf?)

(defn qtree-str [math-inner math-leaves node]
  "Converts `node` into a qtree string.
`math-inner` and `math-leaves` are booleans that indicate whether labels
of inner and leaf nodes should be enclosed in $s, respecively."
  (let [children   (:children node)
        child-strs (map (partial qtree-str math-inner math-leaves) children)
        label      (:label node)
        math       (or (and (leaf? node) math-leaves)
                       (and (not (leaf? node)) math-inner))
        wrap       (fn [label] (cond
                                 math (str "$" label "$")
                                 (re-find #"\s" label) (str "{" label "}")
                                 :else label))]
    (if (empty? children) ; if the node is a leaf
      (wrap label)
      (str "[." (wrap label) " " (str/join " " child-strs) " ]"))))

(defn get-output-str-qtree []
  "Returns the qtree string representation of the forest."
  (let [db @db
        math-inner (:math-inner db)
        math-leaves (:math-leaves db)]
    (str/join "\n" (map (partial qtree-str math-inner math-leaves)
                        (:forest db)))))

(defn tree-json [node]
  "Converts `node` into a json-like exportable object"
  (let [children (:children node)
        label (:label node)]
    {:label label
     :children (mapv tree-json children)}))

(defn get-output-str-json []
  (let [db @db
        forest (:forest db)
        indent (if (:pretty-print-json db) 2 0)]
    (str/join "\n\n" (map #(.stringify js/JSON (clj->js (tree-json %)) nil indent) forest))))

(defn math-inner? []
  (:math-inner @db))

(defn math-leaves? []
  (:math-leaves @db))

(defn pretty-print-json? []
  (:pretty-print-json @db))

(defn toggle-math-inner! []
  (swap! db update :math-inner not))

(defn toggle-math-leaves! []
  (swap! db update :math-leaves not))

(defn toggle-pretty-print-json! []
  (swap! db update :pretty-print-json not))

;-----------------;
; Forest requests ;
;-----------------;

;; basic node functions
;; --------------------

(def default-node
  {:selected false
   :renaming false
   :label ""
   :children []
   :width 1})

(defn leaf? [node]
  "Returns true iff the node is a leaf."
  (empty? (:children node)))

(defn tree-selected? [node]
  "Returns true iff some node in the tree is selected."
  (if (:selected node)
    true
    (some tree-selected? (:children node))))

(defn update-node [node f index]
  "Updates the node at `index` in the tree under `node` by applying `f` to it.
   `index` represents the path to a node as a sequence of child indices.
   Returns the updated tree."
  (if (empty? index)
    (f node)
    (let [children (update (:children node) (first index) update-node f (rest index))]
      (assoc node :children children))))

(defn update-forest [forest f index]
  "Updates the node at `index` in `forest` by applying `f` to it.
   `index` represents the path to a node as a sequence of child indices.
   Returns the updated forest."
  (if (empty? index)
    forest
    (update forest (first index) update-node f (rest index))))

;; forest requests
;; ---------------

(defn get-forest []
  (:forest @db))

(defn make-leaf [label]
  "Returns a leaf node with label `label`."
  (assoc default-node
         :label label
         :width 1))

(defn set-leaves [leaves]
  "Replaces the forest with a list of unconnected leaves."
  (swap! db assoc :forest (mapv make-leaf leaves)))

;; selection
;; ---------

;; toggle

(defn toggle-select! [index]
  "Toggles the selection state of the node at `index`.
   `index` represents the path to a node as a sequence of child indices."
  (letfn [(toggle [node]
            (update node :selected not))]
    (swap! db update :forest update-forest toggle index)))

;; deselect

(declare deselect-all-trees)

(defn deselect-tree [node]
  "Deselects a node and all its descendants."
  (assoc node
         :selected false
         :children (deselect-all-trees (:children node))))

(defn deselect-all-trees [forest]
  (mapv deselect-tree forest))

(defn deselect-all []
  "Deselects all nodes."
  (swap! db update :forest deselect-all-trees))

;; rename
;; ------

(defn rename-node [label index]
  "Assings the label `label` to the node at `index`.
   `index` represents the path to a node as a sequence of child indices."
  (letfn [(rename [node]
            (assoc node :label label))]
    (swap! db update :forest update-forest rename index)))

(defn start-renaming-node [index]
  "Puts the node at `index` into renaming mode.
   `index` represents the path to a node as a sequence of child indices."
  (letfn [(start-renaming [node]
            (assoc node :renaming true))]
    (swap! db update :forest (comp deselect-all-trees update-forest) start-renaming index)))

(defn stop-renaming-node [index]
  "Puts the node at `index` out of renaming mode.
   `index` represents the path to a node as a sequence of child indices."
  (letfn [(stop-renaming [node]
            (assoc node :renaming false
                        :selected true))]
    (swap! db update :forest update-forest stop-renaming index)))

;; combine
;; -------

(defn combine [children]
  "Combines the sequence `children` into a new tree."
  (let [label (:label (last children))]
    (assoc default-node
           :selected true
           :label label
           :children (deselect-all-trees children)
           :width (reduce + (map :width children)))))

(defn combine-selected-trees
  ([forest]
   "Finds groups of adjacent selected trees in `forest` and combines them."
   (combine-selected-trees forest [] []))
  ([forest group done-forest]
   ;; `forest` is the part of the forest still to be processed.
   ;; `group` is the current group of selected trees.
   ;; `done-forest` is the part of the forest that has already been processed.
   (letfn [(combine+add-group [done-forest group]
             ;; if `group` is non-empty, combine it and add it to `done-forest`
             (if (empty? group)
               done-forest
               (conj done-forest (combine group))))]
     (if (empty? forest)
       ;; end of forest: combine the remaining group
       (combine+add-group done-forest group)
       ;; in forest: look at next tree
       (let [tree (first forest)
             tail (rest forest)]
         (if (tree-selected? tree)
           ;; selected? -> add to group of selected trees
           (combine-selected-trees tail (conj group tree) done-forest)
           ;; not selected? -> combine group and continue
           (combine-selected-trees tail [] (conj (combine+add-group done-forest group)
                                                 tree))))))))

(defn combine-selected []
  "Finds groups of adjacent selected trees and combines them."
  (swap! db update :forest combine-selected-trees))

;; delete
;; ------

(defn node-delete-selected [node]
  "Deletes all selected descendants of `node` (including `node` itself) and their ancestors.
   Returns either the unchanged node or, if any node was deleted,
   a list of remaining subtrees."
  (let [subtrees (mapv node-delete-selected (:children node))]
    (if (or (and (:selected node) (not (leaf? node))) ; node selected?     -> delete node
            (not-every? map? subtrees))               ; any child deleted? -> delete node
      (vec (flatten subtrees))                        ; delete node by returning chilren
      node)))                                         ; keep node

(defn delete-selected []
  "Delete all selected nodes and their ancestors."
  (letfn [(delete-sel [forest]
            (vec (flatten (map node-delete-selected forest))))]
    (swap! db update :forest delete-sel)))

;; parse
;; -----

(defparser qtree-parser "
  forest   = node*
  node     = (label | <'[.'> label children <']'>) <#'\\s*'>
  children = node+
  label    = (group | math | string) <#'\\s*'>
  group    = <'{'> #'[^{}]*' (group #'[^{}]*')* <'}'>
  math     = <'$'> #'[^\\$\\[\\]]+' <'$'>
  string   = #'[^\\s{\\[\\$]+'
")

(defn parse-group [group]
  "Converts a parse (nested vector) of a group (\"{...}\") to a string."
  (if (string? group)
    group
    (str "{" (apply str (map parse-group (rest group))) "}")))

(defn parse-label [strip-math parse]
  "Converts a parse of a label (group, math or non-whitespace string) into a string.
If `strip-math` is `true`, math labels will not have $s."
  (let [content (second parse)]
    (case (first content)
      :string (second content)
      :math (if strip-math
              (second content)
              (str "$" (second content) "$"))
      :group (parse-group content)
      "empty")))

(defn tree-from-parse [strip-math parse]
  "Converts a parse of a tree into a tree representation."
  (let [label (parse-label strip-math (nth parse 1 "empty"))
        children (vec (rest (nth parse 2 [])))]
    (assoc default-node
           :label label
           :children (mapv (partial tree-from-parse strip-math) children))))

(defn load-tree-string []
  "Replaces the forest by the forest parsed from the qtree input string."
  (swap! db (fn [db]
              (let [parse (qtree-parser (:input-tree-str db))
                    strip-math (:strip-math db)
                    forest' (mapv (partial tree-from-parse strip-math) (vec (next parse)))]
                (assoc db :forest forest')))))

;---------------------;
; visibility requests ;
;---------------------;

(defn show-manual? []
  (@db :show-manual))

(defn toggle-manual! []
  (swap! db update :show-manual not))

(defn show-io? []
  (@db :show-io))

(defn toggle-io! []
  (swap! db update :show-io not))
