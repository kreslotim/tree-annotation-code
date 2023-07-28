;-------------------------;
; Database state managing ;
;-------------------------;

(ns tree-annotation.database
  (:require [reagent.core :as r] 
            [clojure.walk :as walk]
            [clojure.string :as str]
            [cljs.reader :as reader] 
            [instaparse.core :as insta :refer-macros [defparser]]))

; private annotation does currently not work in clojure script
(defonce ^:private db (r/atom 
  {:input-str         "Dm G7 C"
   :input-qtree-str   "[.$C$ [.$G7$ $Dm$ $G7$ ] $C$ ] "
   :input-json-str    "{
  \"label\": \"C\",
  \"children\": [
    {
      \"label\": \"G7\",
      \"children\": [
        {
          \"label\": \"Dm\",
          \"children\": []
        },
        {
          \"label\": \"G7\",
          \"children\": []
        }
      ]
    },
    {
      \"label\": \"C\",
      \"children\": []
    }
  ]
}"
   :math-inner        true
   :math-leaves       true
   :math-tree         false
   :tree-reversed     false
   :pretty-print-json false
   :strip-math        true
   :forest            []
   :tab               nil
   :split-arity       2
   :previous-states   []
   :redone-states     []}))


;----------------------;
; Undo & Redo requests ;
;----------------------;

(defn push-current-state []
  "Pushes the current state onto the undo stack and cleares the redo stack."
  (swap! db update :previous-states conj @db)
  (swap! db assoc :redone-states []))

(defn undo []
  "Undoes the last action."
  (let [previous-states (:previous-states @db)]
    (when (not-empty previous-states)
      (let [last-state (peek previous-states)
            new-previous-states (pop previous-states)]
        ;; Before undoing, add current state to redone-states
        (swap! db assoc :redone-states (conj (:redone-states @db) @db))
        ;; Undo, and update previous-states (with last element popped out)
        (swap! db assoc :forest (:forest last-state)) 
        (swap! db assoc :previous-states new-previous-states)))))

(defn redo []
  "Redoes the last undone action."
  (let [redone-states (:redone-states @db)]
    (when (not-empty redone-states)
      (let [last-undone-state (peek redone-states)
            new-redone-states (pop redone-states)]
        ;; Before redoing, add current state to previous-states
        (swap! db assoc :previous-states (conj (:previous-states @db) @db))
        ;; Redo, and update redone-states (with last element popped out)
        (swap! db assoc :forest (:forest last-undone-state))
        (swap! db assoc :redone-states new-redone-states)))))


;-----------------------;
; Input string requests ;
;-----------------------;

(defn get-input-str []
  (:input-str @db))

(defn set-input-str [input-str]
  (swap! db assoc :input-str (str/trim-newline input-str)))

(defn get-split-arity []
  (:split-arity @db))

(defn set-split-arity [arity]
  (swap! db assoc :split-arity arity))

;----------------------------;
; Input tree string requests ;
;----------------------------;

;; qtree

(defn get-input-qtree-str []
  (:input-qtree-str @db))

(defn set-input-qtree-str [input-tree-str]
  (swap! db assoc :input-qtree-str (str/trim-newline input-tree-str)))

(defn strip-math? []
  (:strip-math @db))

(defn toggle-strip-math! []
  (swap! db update :strip-math not))

;; JSON

(defn get-input-json-str []
  (:input-json-str @db))

(defn set-input-json-str [input-tree-str]
  (swap! db assoc :input-json-str (str/trim-newline input-tree-str)))

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

(defn get-json-str [db indent]
  (let [forest (:forest db)
        json-forest (mapv tree-json forest)
        top (if (= (count json-forest) 1)
              (first json-forest)
              json-forest)]
    (.stringify js/JSON (clj->js top) nil indent)))

(defn get-output-str-json []
  (let [db @db
        indent (if (:pretty-print-json db) 2 0)]
    (get-json-str db indent)))

(defn math-inner? []
  (:math-inner @db))

(defn math-leaves? []
  (:math-leaves @db))

(defn math-tree? []
  (:math-tree @db))

(defn tree-reverse? []
  (:tree-reversed @db))

(defn pretty-print-json? []
  (:pretty-print-json @db))

(defn toggle-math-tree! []
  (swap! db update :math-tree not))

(defn toggle-tree-direction! []
  (swap! db update :tree-reversed not))

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

  ;; Save the current state
  (push-current-state)

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

(defn start-renaming-selected []
  "Puts all selected nodes in renaming mode."
  (letfn [(start-renaming-if-selected [node]
            (assoc node
                   :renaming (:selected node)
                   :selected false
                   :children (mapv start-renaming-if-selected (:children node))))]
    (swap! db update :forest #(mapv start-renaming-if-selected %))))


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

  ;; Save the current state
  (push-current-state)

  (swap! db update :forest combine-selected-trees))


;; elaborate
;; ---------

(defn create-node [label]
  "Creates a new node with a given label."
  (assoc default-node
         :label label 
         :selected false
         :width 1))

(defn elaborate-node [node]
  "If the node is a leaf and it is selected, adds `split-arity` children labeled '*'.
   Otherwise, applies `elaborate-node` to all its children."
  (if (and (leaf? node) (tree-selected? node))
    (let [new-children (repeat (:split-arity @db) (create-node "*"))]
      (assoc node :children new-children))
    (assoc node :children (map elaborate-node (:children node)))))


(defn elaborate-selected []
  "Finds selected trees and elaborates them."

  ;; Save the current state
  (push-current-state)

  (swap! db update :forest (fn [forest] (map elaborate-node forest)))
  (deselect-all))


;; uncombine
;; ---------

(defn node-uncombine-selected [node]
  "Deletes all selected descendants of `node` (including `node` itself) and their ancestors.
   Returns either the unchanged node or, if any node was deleted,
   a list of remaining subtrees."
  (let [subtrees (mapv node-uncombine-selected (:children node))]
    (if (or (and (:selected node) (not (leaf? node))) ; node selected?     -> delete node
            (not-every? map? subtrees))               ; any child deleted? -> delete node
      (vec (flatten subtrees))                        ; delete node by returning chilren
      node)))                                         ; keep node

(defn uncombine-selected []
  "Delete all selected nodes and their ancestors, if not a leaf."

  ;; Save the current state
  (push-current-state)

  (letfn [(delete-sel [forest]
            (vec (flatten (map node-uncombine-selected forest))))]
    (swap! db update :forest delete-sel)))


;; unelaborate
;; -----------

(defn node-unelaborate-selected [node]
  "If `node` is selected, deletes all its children.
   Returns the unchanged node or the node without children if it was selected."
  (if (:selected node)
    (assoc node :children [])  ;; Remove children by setting :children to empty list
    (assoc node :children (mapv node-unelaborate-selected (:children node)))))  ;; Otherwise, recursively check children

(defn unelaborate-selected []
  "Removes all children of selected nodes."

  ;; Save the current state
  (push-current-state)

  ;; Update the forest
  (swap! db update :forest (fn [forest]
                             (mapv node-unelaborate-selected forest))))



;; delete
;; ------

(defn node-delete-selected [node]
  "Deletes all selected descendants of `node` (including `node` itself) and their ancestors.
   Returns either the unchanged node or, if any node was deleted,
   a list of remaining subtrees."
  (let [subtrees (mapv node-delete-selected (:children node))]
    (if (or (:selected node)            ; node selected?     -> delete node
            (not-every? map? subtrees)) ; any child deleted? -> delete node
      (vec (flatten subtrees))          ; delete node by returning chilren
      node)))                           ; keep node

(defn delete-selected []
  "Delete all selected nodes and their ancestors."

  ;; Save the current state
  (push-current-state)

  (letfn [(delete-sel [forest]
            (vec (flatten (map node-delete-selected forest))))]
    (swap! db update :forest delete-sel)))


;; add-left
;; --------

(defn add-left []
  "Creates a new node with a given label and adds it to the left of the root nodes in the forest."

  ;; Save the current state
  (push-current-state)

  (let [new-node (create-node "*")]  ;; create a new node
    (swap! db update :forest #(vec (cons new-node %)))))  ;; add new node to the beginning of the forest



;; add-right
;; ---------

(defn add-right []
  "Creates a new node with a given label and adds it to the right of the root nodes in the forest."

  ;; Save the current state
  (push-current-state)

  (let [new-node (create-node "*")]  ;; create a new node
    (swap! db update :forest conj new-node)))  ;; add new node to the end of the forest


;; screenshot
;; ----------

(defn save-preview []
  (dotimes [i (.-length (js/document.getElementsByClassName "katex-html"))]
    (.remove (aget (js/document.getElementsByClassName "katex-html") 0)))
  (-> (js/html2canvas (js/document.getElementById "preview"))
      (.then (fn [canvas]
               (doto (js/document.createElement "a")
                 (set! -href (.toDataURL canvas "image/png"))
                 (set! -download "preview.png")
                 .click)))))

(defn save-forest [] 
  (-> (js/html2canvas (js/document.getElementById "forest"))
      (.then (fn [canvas]
               (let [link (js/document.createElement "a")
                     data (.toDataURL canvas "image/png")]
                 (set! (.-href link) data)
                 (set! (.-download link) "forest.png")
                 (.click link))))))

;; parse
;; -----

;;; qtree

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

(defn parse-qtree-forest [qtree-str strip-math]
  (let [parse (qtree-parser qtree-str)]
    (mapv (partial tree-from-parse strip-math) (vec (next parse)))))

(defn load-qtree-string []
  "Replaces the forest by the forest parsed from the qtree input string."
  (swap! db (fn [db]
              (let [qtree-str (:input-qtree-str db)
                    strip-math (:strip-math db)]
                (assoc db :forest (parse-qtree-forest qtree-str strip-math))))))

;;; JSON

(defn tree-from-json [node]
  (let [label    (get node "label")
        children (get node "children")]
    (assoc default-node
           :label label
           :children (mapv tree-from-json children))))

(defn parse-json-forest [json-str]
  (let [json (js->clj (js/JSON.parse json-str))
        top (if (map? json) [json] json) ; tree or forest? -> make forest
        ]
    (mapv tree-from-json top)))

(defn load-json-string []
  "Replaces the forest by the forest given in the JSON input string."
  (swap! db (fn [db]
              (let [json-str (:input-json-str db)]
                (assoc db :forest (parse-json-forest json-str))))))

;;; base64

(defn load-b64-string [b64-str]
  (swap! db assoc :forest (parse-json-forest (js/atob b64-str))))

(defn get-output-str-b64 []
  (js/btoa (get-json-str @db 0)))

;---------------------;
; visibility requests ;
;---------------------;

(defn get-tab []
  (:tab @db))

(defn toggle-tab! [tab]
  (swap! db update :tab #(if (= % tab) nil tab)))

(defn show-manual? []
  (= (get-tab) :manual))

(defn toggle-manual! []
  (toggle-tab! :manual))

(defn show-input? []
  (= (get-tab) :input))

(defn toggle-input! []
  (toggle-tab! :input))

(defn show-output? []
  (= (get-tab) :output))

(defn toggle-output! []
  (toggle-tab! :output))

(defn show-preview? []
  (= (get-tab) :preview))

(defn toggle-preview! []
  (toggle-tab! :preview))
