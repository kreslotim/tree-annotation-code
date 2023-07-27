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


(defn latex-component [label]
  (r/create-class
   {:component-did-mount (fn [this] (js/renderMathInElement (rdom/dom-node this)))
    :component-did-update (fn [this _] (js/renderMathInElement (rdom/dom-node this)))
    :reagent-render (fn [label]
                      (let [single-dollar? (and (clojure.string/starts-with? label "$")
                                                (clojure.string/ends-with? label "$")
                                                (not (clojure.string/starts-with? label "$$"))
                                                (not (clojure.string/ends-with? label "$$")))
                            double-dollar? (and (clojure.string/starts-with? label "$$")
                                                (clojure.string/ends-with? label "$$"))]
                        [:span {:class "latex"} (cond
                                                  single-dollar? (str "$" label "$")
                                                  double-dollar? label
                                                  :else (str "$$" label "$$"))]))}))


(defn node-component [node index]
  "Create a component (a button or text field) from a node."
  (let [label (:label node)
        math-tree? (db/math-tree?)
        input-ref (atom nil)]
    (if (:renaming node)
      (let [set-input-ref (fn [element]
                            (reset! input-ref element)
                            (when element
                              (set! (.-value element) label)
                              (.focus element)))]
        [:input.node
         {:ref set-input-ref
          :type "text"
          :on-change #(db/rename-node (-> % .-target .-value) index)
          :on-blur #(db/stop-renaming-node index)
          :on-key-down (fn [ev]
                         (when (= (.-key ev) "Enter")
                           (db/stop-renaming-node index)))}])
      [:div.node
       {:class (selection-class node)
        :on-click #(db/toggle-select! index)
        :on-double-click #(db/start-renaming-node index)}
       (if (or (clojure.string/starts-with? label "$") math-tree?)
         [latex-component label math-tree?]
         label)])))




;--------------------------------------;
; Tree component and tree manipulation ;
;--------------------------------------;

(defn arity-input-component []
  [:div
   [:div.pure-form.pure-g {:style {:display "flex" :align-items "center" :margin-right "7px" :margin-top "20px"}}
    [:h4 {:style {:letter-spacing "0px"
                  :font-weight "lighter"
                  :margin-right "10px"
                  :margin-left "20px"}} "Split arity"]
    [:input.pure-input-1 
     {:style {:width "60px"}
      :type "number"
      :value (db/get-split-arity)
      :min 1
      :on-change #(db/set-split-arity (-> % .-target .-value js/parseInt))}]]
   [:div.pure-u-1.pure-u-md-3-4]])

(defn mathbox-component []
  [:label.math-box
   [:input
    {:type "checkbox"
     :checked (db/math-tree?)
     :on-change db/toggle-math-tree!}]
   " Math tree"])

(defn tree-reverse-component []
  [:label.reverse-box 
   [:input
    {:type "checkbox"
     :checked (db/tree-reverse?)
     :on-change db/toggle-tree-direction!}]
   " Reverse tree"])

(defn undo-redo-component []
  [:div.undo-redo-buttons 
   [:button.pure-button
    {:on-click db/undo} "↩"]
   [:button.pure-button
    {:on-click db/redo} "↪"]])

(defn tree-component [node index]
  (let [children (:children node)
        length (count children)
        component (node-component node index)
        reversed (db/tree-reverse?)] 
    [:div.subtree
     {:class (if reversed "subtree reversed" "subtree")}
     component
     (into [:div.forest.children]
           (mapv (fn [child i] (tree-component child (conj index i)))
                 children
                 (range length)))]))


(defn tree-annotation-component []
  "Creates a set of components corresponding to the nodes in the database
  and some buttons for interaction."
  [:div
   [:div.content
    [:h2 "Annotation"]
    [:div.pure-button-group.controls
     {:role "group"}
     [undo-redo-component]
     [arity-input-component] 
     [:button.pure-button
      {:on-click (fn [e]
                   (db/elaborate-selected)
                    (.blur (.-currentTarget e)))} "Elaborate"]
     [:button.pure-button.button-uncombine
      {:on-click (fn [e]
                   (db/unelaborate-selected)
                   (.blur (.-currentTarget e)))} "Unelaborate"] 
     [:button.pure-button
      {:on-click (fn [e]
                   (db/combine-selected)
                   (.blur (.-currentTarget e)))} "Combine"]
     [:button.pure-button.button-uncombine
      {:on-click (fn [e]
                   (db/uncombine-selected)
                   (.blur (.-currentTarget e)))} "Uncombine"]
     [:button.pure-button
      {:on-click (fn [e]
                   (db/deselect-all)
                   (.blur (.-currentTarget e)))} "Deselect All"]
     [:button.pure-button.button-delete
      {:on-click (fn [e]
                   (db/delete-selected)
                   (.blur (.-currentTarget e)))} "Delete"]]]
   [:div.wrapper
    [:button.pure-button.button-new-left
     {:on-click (fn [e]
                  (db/add-left)
                  (.blur (.-currentTarget e)))} "⬅"] 
    [:button.pure-button.button-new-right
     {:on-click (fn [e]
                  (db/add-right)
                  (.blur (.-currentTarget e)))} "➡"]
    [tree-reverse-component]
    [mathbox-component]]
   (into
    [:div.tree.forest]
    (let [forest (db/get-forest)
          length (count forest)]
      (mapv (fn [tree i] (tree-component tree [i]))
            forest
            (range length))))])


;------------------------;
; tree preview component ;
;------------------------;

(defn latex-component1 [label]
  (r/create-class
   {:component-did-mount (fn [this] (js/renderMathInElement (rdom/dom-node this)))
    :component-did-update (fn [this _] (js/renderMathInElement (rdom/dom-node this)))
    :reagent-render (fn [label] [:span {:class "latex"} label])}))

(def svg-scale 50)

(defn svg-align-subtrees [subtrees]
  "takes a list of subtree elements and returns a new element with aligned subtrees"
  (let [maxh (reduce max (map (comp :h :coords) subtrees))
        [svgs wtotal]
        (reduce (fn [[elts wtotal] {{w :w h :h} :coords subtree :svg}]
                  (let [x wtotal
                        y (inc (- maxh h))
                        svg [:svg
                             {:x (* svg-scale x) :y (* svg-scale y)
                              :style {:overflow "visible"}}
                             subtree]
                        elt {:coords {:x x :y y :w w :h h}
                             :svg svg}
                        elts' (conj elts elt)]
                    [elts' (+ wtotal w)]))
                [[] 0]
                subtrees)]
    {:coords {:w wtotal :h maxh}
     :svg (into [:g] (map :svg svgs))
     :child-coords (mapv :coords svgs)}))

(defn svg-child-line [w h {xc :x wc :w hc :h}]
  [:line {:x1 (* svg-scale (/ (dec w) 2)) :y1 0
          :x2 (* svg-scale (+ xc (/ (dec wc) 2))) :y2 (* svg-scale (- (inc h) hc))
          :stroke "black"}])

(defn svg-label [label x y]
  (let [position {:x (* svg-scale x) :y (* svg-scale y)}]
    (if (db/math-tree?)
      [:g 
       [:text {:style {:visibility "hidden"} :x (:x position) :y (:y position)
               :text-anchor "middle"
               :dominant-baseline "middle"
               :filter "url(#clear)"} label]
       [:foreignObject {:x (+ (:x position) -49) :y (+ (:y position) -12)
                        :width 100 :height 20} ;; Arbitrary width/height
        [latex-component label]]]
      [:text (merge position {:text-anchor "middle" :dominant-baseline "middle"
                              :filter "url(#clear)"}) label])))




(defn svg-subtree [node]
  (let [children (:children node)
        label (:label node)
        subtrees (map svg-subtree children)
        coords (map :coords subtrees)
        {{w :w h :h} :coords children-svg :svg child-coords :child-coords}
        (svg-align-subtrees subtrees)]
    (if (empty? children)
      ;; leaf
      {:coords {:w 1 :h 1}
       :svg (svg-label label 0 0)}
      ;; inner node
      {:coords {:w w :h (inc h)}
       :svg
       [:svg {:style {:overflow "visible"}}
        (into [:g] (map (partial svg-child-line w h) child-coords)) 
        (svg-label label (* svg-scale (/ (dec w) 100)) 0)
        children-svg]})))


(defn svg-tree-component []
  (let [forest (db/get-forest)
        subtrees (mapv svg-subtree forest)
        {{w :w h :h} :coords trees-svg :svg} (svg-align-subtrees subtrees)
        width (* svg-scale (+ w 2))
        height (* svg-scale (+ h 2))
        svg (into
             [:svg {:width width :height height
                    :viewBox [(- svg-scale) (- svg-scale) width height]
                    :style {:overflow "visible"}}
              [:defs [:filter {:x 0 :y 0 :width 1 :height 1 :id "clear"}
                      [:feFlood {:flood-color "white"}]
                      [:feComposite {:in "SourceGraphic"}]]]]
             trees-svg)]
    [:div#preview.tree 
     (when (db/show-preview?)
       svg)]))




#_(def svg-scale 50)

#_(defn svg-align-subtrees [subtrees]
  "takes a list of subtree elements and returns a new element with aligned subtrees"
  (let [maxh (reduce max (map (comp :h :coords) subtrees))
        [svgs wtotal]
        (reduce (fn [[elts wtotal] {{w :w h :h} :coords subtree :svg}]
                  (let [x wtotal
                        y (inc (- maxh h))
                        svg [:svg
                             {:x (* svg-scale x) :y (* svg-scale y)
                              :style {:overflow "visible"}}
                             subtree]
                        elt {:coords {:x x :y y :w w :h h}
                             :svg svg}
                        elts' (conj elts elt)]
                    [elts' (+ wtotal w)]))
                [[] 0]
                subtrees)]
    {:coords {:w wtotal :h maxh}
     :svg (into [:g] (map :svg svgs))
     :child-coords (mapv :coords svgs)}))

#_(defn svg-child-line [w h {xc :x wc :w hc :h}]
  [:line {:x1 (* svg-scale (/ (dec w) 2)) :y1 0
          :x2 (* svg-scale (+ xc (/ (dec wc) 2))) :y2 (* svg-scale (- (inc h) hc))
          :stroke "black"}])


#_(defn svg-label [label x y]
    [:text {:x x :y y
            :text-anchor "middle"
            :dominant-baseline "middle"
            :filter "url(#clear)"}
     label])

#_(defn svg-subtree [node]
    (let [children (:children node)
          label (:label node)
          subtrees (map svg-subtree children)
          coords (map :coords subtrees)
          {{w :w h :h} :coords children-svg :svg child-coords :child-coords}
          (svg-align-subtrees subtrees)]
      (if (empty? children)
      ;; leaf
        {:coords {:w 1 :h 1}
         :svg (svg-label label 0 0)}
      ;; inner node
        {:coords {:w w :h (inc h)}
         :svg
         [:svg {:style {:overflow "visible"}}
          (into [:g] (map (partial svg-child-line w h) child-coords))
          (svg-label label (* svg-scale (/ (dec w) 2)) 0)
          children-svg]})))

#_(defn svg-tree-component []
  (let [forest (db/get-forest)
        subtrees (mapv svg-subtree forest)
        {{w :w h :h} :coords trees-svg :svg} (svg-align-subtrees subtrees)
        width (* svg-scale (+ w 2))
        height (* svg-scale (+ h 2))
        svg (into
             [:svg {:width width :height height
                    :viewBox [(- svg-scale) (- svg-scale) width height]
                    :style {:overflow "visible"}}
              [:defs [:filter {:x 0 :y 0 :width 1 :height 1 :id "clear"}
                      [:feFlood {:flood-color "white"}]
                      [:feComposite {:in "SourceGraphic"}]]]]
             trees-svg)]
    [:div#preview.tree
     (when (db/show-preview?)
       svg)]))







#_(defn svg-label [label x y math-tree?]
    (let [latex-label (str "$$" label "$$")  ;; or use logic from latex-component
          html (if (or (clojure.string/starts-with? label "$") math-tree?)
                 [:body [:span {:class "latex"} latex-label]]
                 [:body label])]
      [:foreignObject {:x x :y y :width "auto" :height "auto"}
       html]))

#_(defn svg-subtree [node math-tree?]
    (let [children (:children node)
          label (:label node)
          subtrees (map svg-subtree children math-tree?)
          coords (map :coords subtrees)
          {{w :w h :h} :coords children-svg :svg child-coords :child-coords}
          (svg-align-subtrees subtrees)]
      (if (empty? children)
      ;; leaf
        {:coords {:w 1 :h 1}
         :svg (svg-label label 0 0 math-tree?)}
      ;; inner node
        {:coords {:w w :h (inc h)}
         :svg
         [:svg {:style {:overflow "visible"}}
          (into [:g] (map (partial svg-child-line w h) child-coords))
          (svg-label label (* svg-scale (/ (dec w) 2)) 0 math-tree?)
          children-svg]})))

#_(defn svg-tree-component []
    (r/create-class
     {:component-did-mount (fn [this] (js/renderMathInElement (rdom/dom-node this)))
      :component-did-update (fn [this _] (js/renderMathInElement (rdom/dom-node this)))
      :reagent-render
      (fn []
        (let [forest (db/get-forest)
              math-tree? (db/math-tree?)
              subtrees (mapv (fn [node] (svg-subtree node math-tree?)) forest)
              {{w :w h :h} :coords trees-svg :svg} (svg-align-subtrees subtrees)
              width (* svg-scale (+ w 2))
              height (* svg-scale (+ h 2))
              svg (into
                   [:svg {:width width :height height
                          :viewBox [(- svg-scale) (- svg-scale) width height]
                          :style {:overflow "visible"}}
                    [:defs [:filter {:x 0 :y 0 :width 1 :height 1 :id "clear"}
                            [:feFlood {:flood-color "white"}]
                            [:feComposite {:in "SourceGraphic"}]]]]
                   trees-svg)]
          [:div#preview.tree
           (when (db/show-preview?)
             svg)]))}))

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
   [:h3 "QTree String"]
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
     [:div.tab
      ;;[:h2 "Input"]
      [sequence-input-component]
      [tree-input-component]
      ])
   #_[:a {:on-click db/toggle-input! :href "javascript:void(0)"} ; void() is used as a dummy href
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

(def base-url "https://dcmlab.github.io/tree-annotation-code/")

(defn link-output-component []
  (let [out-str-b64 (db/get-output-str-b64)
        href (str base-url "?tree=" out-str-b64)]
    [:div
     [:h3 "Share this Tree"]
     [:a {:href href} "Link to This Tree"]]))

(defn qtree-output-component []
  (let [out-str-qtree (db/get-output-str-qtree)]
    [:div.pure-form.pure-g
     [:h3.pure-u-1 "QTree String"]
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
     [:div.pure-u-1.pure-u-md-3-4]
     [:button.pure-button.pure-button-primary.pure-u-1.pure-u-md-1-4
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
     [:div.pure-u-1.pure-u-md-3-4]
     [:button.pure-button.pure-button-primary.pure-u-1.pure-u-md-1-4
      {:on-click #(copy-to-clipboard out-str-json)}
      "Copy to Clipboard"]]))

(defn output-component []
  [:div
   (when (db/show-output?)
     [:div.tab
      ;;[:h2 "Output"]
      (when (> (count (db/get-forest)) 1)
        [:div.alert "Warning: tree is incomplete!"])
      [link-output-component]
      [qtree-output-component]
      [json-output-component]])
   #_[:button.pure-button {:on-click db/toggle-output!} ; void() is used as a dummy href
       (if (db/show-output?) "Hide Output" "Show Output")]])

;------------------;
; Manual component ;
;------------------;

(def manual-string "

## Manual

by [Daniel Harasim](https://people.epfl.ch/daniel.harasim),
[Christoph Finkensiep](https://people.epfl.ch/christoph.finkensiep),
and the [Digital and Cognitive Musicology Lab (DCML)](https://dcml.epfl.ch)

This is an open source project. Find the code [here](https://github.com/DCMLab/tree-annotation-code).

### Quickstart

1. Write the sequence that you want to annotate with a tree into the text field.
   The elements of the sequence must be separated by space characters.
1. Press the *load sequence* button to load the sequence as leaf nodes of a tree.
1. Select nodes that you want to combine by clicking on them.
   Clicking again deselects a selected node.
   If the node is not a root, the path to the root will be highlighted too, in a different color.
1. Press `Enter` or `c` (or click on `Combine`) to combine the selected subtrees into a new tree.
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
  Submit the new name by pressing `Enter`. Pressing `r` opens a text field for every selected node.
- Pressing `e` (or clicking the `Elaborate` button) generates child nodes from selected nodes,
  quantity determined by `Split arity`.
- Pressing `u` or `Ctl+E` (or clicking the `Unelaborate` button) unelaborates all selected nodes, by deleting their descendants.
- Pressing `Backspace` or `Ctrl+U` (or clicking the `Uncombine` button) uncombines all selected nodes, 
  removing their ancestors, unless the node is a leaf. 
  (Only inner nodes or the last leaf node can be uncombined.)
- Pressing `Delete` (or clicking the `Delete` button)
  deletes all selected nodes and their ancestors.
- Pressing `Esc` (or clicking the `Deselect All` button) deselects all nodes.
- Pressing `Ctrl+Z` (or clicking the `↩` button) undoes the last changes.
- Pressing `Ctrl+Y` (or clicking the `↪` button) redoes the last undone changes.
- Use `⬅️` (Left Arrow key) to create a new root node on the tree's left, or `➡️` (Right Arrow key) for a right-side root node.
- By selecting `Reverse tree` option, you can flip the orientation of the tree, effectively rendering it upside down.
- By selecting `Math tree` option, you enable the rendering of all tree content using LaTeX format.   
- Pressing `i` or `o` toggles the input or output section, respectively.
  Pressing `m`, `h`, or `?` toggles the manual section.
  Pressing `p` toggles the preview section.
- You can also edit an existing qtree string by loading it 
  using the *load qtree string* button.

")

(defn manual-component []
  [:div
   (when (db/show-manual?)
     [:div.manual.tab
      {:style {:line-height "1.5"}}
      (md/md->hiccup manual-string)])
   #_[:a {:on-click db/toggle-manual! :href "javascript:void(0)"} ; void() is used as a dummy href
    (if (db/show-manual?) "Hide Manual" "Show Manual")]])

;---------------;
; App component ;
;---------------;

(defn unfocus [action!]
  (fn [ev]
    (.. ev -target blur)
    (action!)))

(defn tab-component []
  [:div.pure-menu.pure-menu-horizontal
   [:ul.pure-menu-list
    [:li.pure-menu-item
     {:class (if (db/show-input?) "pure-menu-selected" "")}
     [:a.pure-menu-link
      {:on-click (unfocus db/toggle-input!) :href "javascript:;"}
      "Input"]]
    [:li.pure-menu-item
     {:class (if (db/show-output?) "pure-menu-selected" "")}
     [:a.pure-menu-link
      {:on-click (unfocus db/toggle-output!) :href "javascript:;"}
      "Output"]]
    [:li.pure-menu-item
     {:class (if (db/show-preview?) "pure-menu-selected" "")}
     [:a.pure-menu-link
      {:on-click (unfocus db/toggle-preview!) :href "javascript:;"}
      "Preview"]][:li.pure-menu-item
     {:class (if (db/show-manual?) "pure-menu-selected" "")}
     [:a.pure-menu-link
      {:on-click (unfocus db/toggle-manual!) :href "javascript:;"}
      "Help"]]]])

(defn app-component []
  [:div
   [:div.content
    [:h1 "Tree Annotation"]
    [tab-component]
    [manual-component]
    [input-component]
    [output-component]]
   [svg-tree-component]
   [tree-annotation-component]
   [:div.bottom-whitespace]])

(defn render []
  (let [params (new js/URLSearchParams. (.. js/window -location -search))
        tree (.get params "tree")]
    (when tree
      (db/load-b64-string tree)
      (db/toggle-preview!)))
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
            "c" (db/combine-selected)
            "Escape" (db/deselect-all)
            "Backspace" (db/uncombine-selected)
            "Delete" (db/delete-selected)
            "i" (db/toggle-input!)
            "o" (db/toggle-output!)
            "?" (db/toggle-manual!)
            "m" (db/toggle-manual!)
            "h" (db/toggle-manual!)
            "p" (db/toggle-preview!)
            "e" (db/elaborate-selected)
            "u" (db/unelaborate-selected)
            "r" (db/start-renaming-selected) 
            "ArrowLeft" (db/add-left)
            "ArrowRight" (db/add-right)
            nil)

          ;; If ctrl key was pressed
          (when (.-ctrlKey event)
            (case (.-key event)
              "z" (do (db/undo) (.preventDefault event))  ;; Prevent default 'undo' action in browser
              "y" (do (db/redo) (.preventDefault event))  ;; Prevent default 'redo' action in browser
              "u" (do (db/uncombine-selected) (.preventDefault event))
              "e" (do (db/unelaborate-selected) (.preventDefault event))
              nil))
          
          (.preventDefault event)
          (.stopPropagation event)
          false)))
