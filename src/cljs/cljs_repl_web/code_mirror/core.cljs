(ns cljs-repl-web.code-mirror.core
  (:require [cljs.repl :as cljs-repl]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [subscribe dispatch]]
            [cljs-repl-web.code-mirror.handlers :as handlers]
            [cljs-repl-web.code-mirror.subs :as subs]
            [cljs-repl-web.code-mirror.editor :as editor]
            [cljs-repl-web.code-mirror.common :as common]
            [cljs-repl-web.code-mirror.utils :as utils]
            [replumb.core :as replumb]
            [re-complete.utils :as complete-utils]
            [re-complete.app :as complete-app]))

;;; many parts are taken from jaredly's reepl
;;; https://github.com/jaredly/reepl

(defn make-handlers [console-key]
  {:add-input    #(dispatch [:add-console-input console-key %1 %2])
   :add-result   #(dispatch [:add-console-result console-key %1 %2])
   :go-up        #(dispatch [:console-go-up console-key %])
   :go-down      #(dispatch [:console-go-down console-key %])
   :clear-items  #(dispatch [:clear-console-items console-key %])
   :set-text     #(dispatch [:console-set-text console-key %])
   :add-log      #(dispatch [:add-console-log console-key %])})

(defn display-output-item
  ([console-key value]
   (display-output-item console-key value false))
  ([console-key value error?]
   [:div
    {:on-click #(dispatch [:focus-console-editor console-key])
     :class (str "cm-console-item" (when error? " error-cm-console-item"))}
    value]))

(defn display-repl-item
  [console-key item]
  (if-let [text (:text item)]
    [:div.cm-console-item
     {:on-click #(do (dispatch [:console-set-text console-key text])
                     (dispatch [:focus-console-editor console-key]))}
     [utils/colored-text (str (:ns item) "=> " text)]]

    (if (= :error (:type item))
      (display-output-item console-key (.-message (:value item)) true)
      (display-output-item console-key (:value item)))))

(defn repl-items [console-key items]
  (into [:div] (map (partial display-repl-item console-key) items)))

(defn opening-excluded-chars [word excluded-chars]
  (if ((set (map #(= (first word) %) excluded-chars)) true)
    (opening-excluded-chars (apply str (rest word)) excluded-chars)
    word))

(defn closing-excluded-chars [word excluded-chars]
  (if ((set (map #(= (last word) %) excluded-chars)) true)
    (closing-excluded-chars (apply str (butlast word)) excluded-chars)
    word))

(defn read-apropos [text excluded-chars]
  (let [new-text (-> text
                     (opening-excluded-chars excluded-chars)
                     (closing-excluded-chars excluded-chars))]
    (.log js/console (str "new-text: " new-text))
    (if (= new-text "")
      ""
      (replumb/read-eval-call
       (fn [result]
         (when (:success? result)
           (->> result
                :value
                cljs.reader/read-string
                (map str))))
       (str "(apropos \"" new-text "\")")))))

(defn create-aprop [excluded-chars libs text]
  (remove nil?
          (map (fn [prop-item]
                 (let [split-prop-item (clojure.string/split prop-item #"/")]
                   (when ((set libs) (first split-prop-item))
                     (second split-prop-item))))
               (read-apropos text excluded-chars))))

(defn current-word [previous-input input]
  (->> input
       (complete-app/index previous-input)
       (complete-app/current-word input)))

(defn console [console-key eval-opts]
  (let [{:keys [add-input
                add-result
                go-up
                go-down
                clear-items
                set-text
                add-log]} (make-handlers console-key)

        {:keys [get-prompt
                should-eval
                evaluate]} eval-opts

        items (subscribe [:get-console-items console-key])
        previous-input  (subscribe [:get-console-current-text console-key])
        text  (subscribe [:get-previous-input console-key])
        new-input (subscribe [:get-previous-input console-key])
        options (subscribe [:get-options console-key])
        submit (fn [source]
                 (evaluate #(dispatch [:on-eval-complete console-key %])
                           source))]
    (reagent/create-class
     {:reagent-render
      (fn []
        [:div.cm-console
         {:on-click #(dispatch [:focus-console-editor console-key])}
         [repl-items console-key @items]
         [editor/editor
          text
          (merge
           editor/default-cm-opts
           {:on-up go-up
            :on-down go-down
            :on-change #(do (set-text %)
                            (dispatch [:dictionary console-key (create-aprop (:trim-chars @options) '("cljs.core") (current-word @previous-input %))])
                            (dispatch [:input console-key %]))
            :on-eval submit
            :get-prompt get-prompt
            :should-eval should-eval})]])
      :component-did-update
      (fn [this]
        (common/scroll-to-el-bottom! (.-parentElement (reagent/dom-node this))))})))
