;-------------------------;
; Database state managing ;
;-------------------------;

(ns tree-annotation.database
  (:require [reagent.core :as r]
            [instaparse.core :as insta :refer-macros [defparser]]))

; private annotation does currently not work in clojure script
(defonce ^:private db (r/atom 
  {:input-str      "Dm G7 C"
   :input-tree-str "[.$C$ [.$G7$ $Dm$ $G7$ ] $C$ ] "
   :math-inner     true
   :math-leaves    true
   :strip-math     false
   :forest         []
   :show-manual    true
  }))

;-----------------------;
; Input string requests ;
;-----------------------;

(defn get-input-str []
  (:input-str @db))

(defn set-input-str [input-str]
  (swap! db assoc :input-str input-str))

;----------------------------;
; Input tree string requests ;
;----------------------------;

(defn get-input-tree-str []
  (:input-tree-str @db))

(defn set-input-tree-str [input-tree-str]
  (swap! db assoc :input-tree-str input-tree-str))

(defn strip-math? []
  (:strip-math @db))

(defn toggle-strip-math! []
  (swap! db update :strip-math not))

;------------------------;
; Output string requests ;
;------------------------;

(declare leaf?)

(defn tree-str [math-inner math-leaves node]
  (let [children (:children node)
        label    (:label node)
        math     (or (and (leaf? node) math-leaves)
                     (and (not (leaf? node)) math-inner))
        wrap     (fn [s] (if math (str "$" s "$") s))]
    (if (empty? children) ; if the node is a leaf
      (str (wrap label) " ")
      (apply str
             (concat ["[." (wrap label) " "]
                     (map (partial tree-str math-inner math-leaves) children)
                     ["] "])))))

(defn get-output-str []
  (let [math-inner (:math-inner @db)
        math-leaves (:math-leaves @db)]
    (apply str (map (partial tree-str math-inner math-leaves)
                    (:forest @db)))))

(defn math-inner? []
  (:math-inner @db))

(defn math-leaves? []
  (:math-leaves @db))

(defn toggle-math-inner! []
  (swap! db update :math-inner not))

(defn toggle-math-leaves! []
  (swap! db update :math-leaves not))

;-----------------------;
; Rename label requests ;
;-----------------------;

;; (defn get-rename-label []
;;   (:rename-label @db))

;; (defn set-rename-label [label]
;;   (swap! db assoc :rename-label label))

;-----------------;
; Forest requests ;
;-----------------;

;; basic node functions
;; --------------------

(defn leaf? [node]
  (empty? (:children node)))

(defn tree-selected? [node]
  (if (:selected node)
    true
    (some tree-selected? (:children node))))

(defn update-node [node f index]
  (if (empty? index)
    (f node)
    (let [children (update (:children node) (first index) update-node f (rest index))]
      (assoc node :children children))))

(defn update-forest [forest f index]
  (if (empty? index)
    forest
    (update forest (first index) update-node f (rest index))))

(defn map-tree [f tree]
  (let [children' (mapv (partial map-tree f) (:children tree))
        tree' (assoc tree :children children')]
    (f tree)))

(defn map-forest [f forest]
  (mapv (partial map-tree f) forest))


(def default-node
  {:selected false
   :renaming false
   :label "empty"
   :children []
   :x 0 :y 0 :width 0})

;; forest requests
;; ---------------

(defn get-forest []
  (:forest @db))

(defn delete-all! []
  (swap! db assoc :forest []))

;; set leaves

(defn make-leaf [label i]
  (assoc default-node
         :label label
         :x i :y 0
         :width 1))

(defn set-leaves! [leaves]
  (swap! db assoc :forest (mapv make-leaf leaves (range (count leaves)))))

;; selection
;; ---------

;; toggle

(defn toggle-select [node]
  (update node :selected not))

(defn toggle-select! [index]
  (swap! db update :forest update-forest toggle-select index))

;; deselect

(defn deselect-tree [node]
  (assoc node
         :selected false
         :children (mapv deselect-tree (:children node))))

(defn deselect-all [forest]
  (mapv deselect-tree forest))

(defn deselect-all! []
  (swap! db update :forest deselect-all))

;; rename
;; ------

;; (defn rename-selected-nodes [node label]
;;   (assoc node
;;          :children (mapv rename-selected-nodes (:children node))
;;          :label (if (:selected node)
;;                   label
;;                   (:label node))))

;; (defn rename-selected [forest label]
;;   (mapv (fn [node] (rename-selected-nodes node label)) forest))

;; (defn rename-selected! [label]
;;   (swap! db update :forest #(rename-selected % label)))

(defn rename-node [node label]
  (assoc node :label label))

(defn rename-node! [label index]
  (swap! db update :forest update-forest #(rename-node % label) index))

(defn start-renaming-selected [forest]
  (map-forest (fn [node]
                (if (:selected node)
                  (assoc node :renaming true :selected false)
                  node))
              forest))

(defn start-renaming-selected! []
  (swap! db update :forest start-renaming-selected))

(defn start-renaming [node]
  (assoc node :renaming true))

(defn start-renaming-node! [index]
  (swap! db update :forest update-forest start-renaming index))

(defn stop-renaming [node]
  (assoc node :renaming false))

(defn stop-renaming-node! [index]
  (swap! db update :forest update-forest stop-renaming index))

;; combine
;; -------

(defn combine [children]
  (let [label (:label (last children))]
    (assoc default-node
           :label label
           :children (mapv deselect-tree children)
           :x (:x (first children))
           :y (inc (reduce max (map :y children)))
           :width (reduce + (map :width children)))))

(defn combine-selected
  ([forest] (combine-selected forest [] []))
  ([forest group acc]
   (letfn [(combine-if [coll xs]
             (if (empty? xs) coll (conj coll (combine xs))))]
     (if (empty? forest)
       ; end of forest
       (combine-if acc group)
       ; in forest
       (let [tree (first forest)
             tail (rest forest)]
         (if (tree-selected? tree)
           (combine-selected tail (conj group tree) acc)
           (combine-selected tail [] (conj (combine-if acc group) tree))))))))

(defn combine-selected! []
  (swap! db update :forest combine-selected))

;; delete
;; ------

(defn node-delete-selected [node]
  "Deletes all selected children of `node`.
Returns either the unchanged node or a list of remaining subtrees."
  (let [subtrees (mapv node-delete-selected (:children node))]
    (if (or (and (:selected node) (not (leaf? node)))
            (not-every? map? subtrees))
      (vec (flatten subtrees))
      node)))

(defn delete-selected [forest]
  "Delete all selected nodes and their ancestors from the forest."
  (vec (flatten (map node-delete-selected forest))))

(defn delete-selected! []
  (swap! db update :forest delete-selected))

;; parse
;; -----

(declare recalc-coords)

(defn recalc-coords-tree [node offset]
  (let [[children' offset-children] (recalc-coords (:children node) offset)
        offset' (if (leaf? node) (inc offset-children) offset-children)
        node' (assoc node
                     :children children'
                     :x offset
                     :y (inc (reduce max 0 (map :y children')))
                     :width (- offset' offset))]
    [node' offset']))

(defn recalc-coords [forest offset]
  (if (empty? forest)
    [[] offset]
    (let [tree (first forest)
          tail (rest forest)
          [tree' offset-children] (recalc-coords-tree tree offset)
          [tail' offset-tail] (recalc-coords tail offset-children)]
      [(into [tree'] tail') offset-tail])))

(defparser qtree-parser "
  forest   = node*
  node     = (label | <'[.'> label children <']'>) <' '?>
  children = node+
  label    = (group | math | string) <' '?>
  group    = <'{'> #'[^{}]*' (group #'[^{}]*')* <'}'>
  math     = <'$'> #'[^$\\[\\]. ]+' <'$'>
  string   = #'\\w+'
")

(defn parse-group [group]
  (if (string? group)
    group
    (str "{" (apply str (map parse-group (rest group))) "}")))

(defn parse-label [strip-math parse]
  (let [content (second parse)]
    (case (first content)
      :string (second content)
      :math (if strip-math
              (second content)
              (str "$" (second content) "$"))
      :group (parse-group content)
      "empty")))

(defn tree-from-parse [strip-math parse]
  (let [label (parse-label strip-math (nth parse 1 "empty"))
        children (vec (rest (nth parse 2 [])))]
    (assoc default-node
           :label label
           :children (mapv (partial tree-from-parse strip-math) children))))

(defn load-tree-string! []
  (swap! db (fn [db]
              (let [parse (qtree-parser (:input-tree-str db))
                    strip-math (:strip-math db)
                    forest' (mapv (partial tree-from-parse strip-math) (vec (next parse)))]
                (js/console.log "parse: " (str parse))
                (assoc db :forest (first (recalc-coords forest' 0)))))))

;-----------------;
; Manual requests ;
;-----------------;

(defn show-manual? []
  (@db :show-manual))

(defn toggle-manual []
  (swap! db #(assoc % :show-manual (not (% :show-manual)))))
