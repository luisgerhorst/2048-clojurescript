(ns ttfe.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            mousetrap))

(enable-console-print!)

(def empty-grid [[nil, nil, nil, nil]
                 [nil, nil, nil, nil]
                 [nil, nil, nil, nil]
                 [nil, nil, nil, nil]])

;; Pure grid functions.

(defn set-last
  "Sets the last item of the vector to value"
  [vector value]
  (if (= (count vector) 0)
    [value]
    (assoc vector (- (count vector) 1) value)))

(defn pair-compressed-row
  "Creates vector with equal fields paired into subvectors"
  [row]
  (reduce (fn [paired field]
            (if (and (= field (last paired)) (number? (last paired)))
              (set-last paired [field field])
              (conj paired field)))
          []
          row))

(defn merge-compressed-row
  [row]
  "Merge field in compressed row (row without empty fields)"
  (reduce (fn [merged field]
            (if (number? field)
              (conj merged field)
              (conj merged (reduce + field))))
          []
          (pair-compressed-row row)))

(defn merge-row
  "Move all field in the row to the left and merge fields with the same value"
  [row]
  (loop [merged-row (merge-compressed-row (filter identity row))]
    (if (< (count merged-row) 4)
      (recur (conj merged-row nil))
      merged-row)))

(defn move-left
  [grid]
  (vec (map merge-row grid)))

(defn move-right
  [grid]
  (vec (map (comp vec rseq) (move-left (vec (map (comp vec rseq) grid))))))

(defn move-top
  [grid]
  (vec (apply map vector (move-left (vec (apply map vector grid))))))

(defn move-bottom
  [grid]
  (vec (apply map vector (move-right (vec (apply map vector grid))))))

(defn get-empty-coordinates
  [grid]
  (loop [empty-fields []
         y 0]
    (if (= y 4)
      empty-fields
      (recur (into empty-fields (loop [empty-fields []
                                       x 0]
                                  (if (= x 4)
                                    empty-fields
                                    (recur (if (get-in grid [y x])
                                             empty-fields
                                             (conj empty-fields [y x])) (inc x))))) (inc y)))))

(defn insert
  [grid]
  (assoc-in grid (rand-nth (get-empty-coordinates grid)) (rand-nth [2 4])))

(defn move-with-function
  [grid function]
  (let [moved (function grid)]
     (if (= moved grid)
       grid
       (insert moved))))

(defn move
  [grid direction]
  (case direction
    :left (move-with-function grid move-left)
    :right (move-with-function grid move-right)
    :top (move-with-function grid move-top)
    :bottom (move-with-function grid move-bottom)))

(defn can-move
  [grid]
  (not (= grid
          (move grid :left)
          (move grid :right)
          (move grid :top)
          (move grid :bottom))))

;; Pure Grid test funcions.

(defn random-block
  [grid]
  (loop [g grid
         i 0]
        (let [moved (move g (rand-nth [:left :right :top :bottom]))]
          (if (or (not moved) (= i 500))
            g
            (recur moved (inc i))))))

;; Pure app state functions.

(defn app-state-reset
  [state]
  (assoc state
    :grid (insert empty-grid)
    :game-over false))

(defn app-state-move
  [state direction]
  (let [moved (move (:grid state) direction)]
    (assoc (if (not (can-move moved))
             (assoc state :game-over true)
             state) :grid moved)))

;; GUI

(def app-state (atom {:grid (insert empty-grid)
                      :game-over false}))

;; Called on user actions.

(defn button-reset
  [app]
  (om/transact! app app-state-reset))

(defn keyboard-move
  [direction]
  (swap! app-state app-state-move direction))

;; Mousetrap

(js/Mousetrap.bind "left" #(keyboard-move :left))
(js/Mousetrap.bind "right" #(keyboard-move :right))
(js/Mousetrap.bind "up" #(keyboard-move :top))
(js/Mousetrap.bind "down" #(keyboard-move :bottom))

;; Om

(om/root
 (fn [app owner]
   (dom/div nil
            ; Grid:
            (apply dom/ul #js {:id "grid"}
                   (map (fn [row]
                          (apply dom/ul #js {:className "row"} (map (fn [value]
                                                                      (if value
                                                                        (dom/li #js {:className "field"} value)
                                                                        (dom/li #js {:className "field"} "")))
                                                                    row)))
                        (:grid app)))
            ; Actions:
            (if (:game-over app)
              (dom/button #js {:id "reset"
                               :onClick #(button-reset app)}
                              "Reset"))))
 app-state
 {:target (. js/document (getElementById "app"))})