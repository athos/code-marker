(ns code-marker.core
  (:refer-clojure :exclude [list])
  (:require [clojure.spec.alpha :as s])
  (:import [clojure.lang RT]
           [java.io LineNumberReader InputStreamReader PushbackReader]))

(def registry (atom {}))

(defn clear-registry []
  (reset! registry {}))

(defmulti inject (fn [mark entry code] mark))
(defmethod inject :default [_ _ code] code)

(def ^:private options
  (atom {:injector inject}))

(defn injector []
  (:injector @options))

(defn set-options! [{:keys [injector] :or {injector :default}}]
  (swap! options
         (fn [options]
           (cond-> options
             (not= injector :default) (assoc :injector injector))))
  nil)

(s/fdef with-mark
  :args (s/cat :mark keyword? :comment (s/? string?) :body (s/* any?)))

(defmacro with-mark [mark & [maybe-comment & body]]
  (let [{:keys [line column]} (meta &form)
        comment (when (string? maybe-comment) maybe-comment)
        body (cond->> body comment (cons comment))
        entry (cond-> {:ns (ns-name *ns*) :file *file* :line line :column column :mark mark}
                comment (assoc :comment comment))]
    (swap! registry assoc-in [mark *file* [line column]] entry)
    (if-let [inject (injector)]
      (inject mark entry `(do ~@body))
      `(do ~@body))))

(defn print-code [{:keys [file line column]}]
  (when-let [strm (.getResourceAsStream (RT/baseLoader) file)]
    (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
      (dotimes [_ (dec line)] (.readLine rdr))
      (.skip rdr (dec column))
      (let [text (StringBuilder. ^String (apply str (repeat (dec column) \space)))
            pbr (proxy [PushbackReader] [rdr]
                  (read [] (let [i (proxy-super read)]
                             (.append text (char i))
                             i)))]
        (read (PushbackReader. pbr))
        (println (str text))))))

(defn marks []
  (set (keys @registry)))

(defn fetch [mark]
  (mapcat vals (vals (get @registry mark))))

(defn query
  ([] (query []))
  ([mark-or-marks]
   (let [marks (cond (keyword? mark-or-marks) [mark-or-marks]
                     (empty? mark-or-marks) (marks)
                     :else mark-or-marks)]
     (mapcat fetch marks))))

(defn list
  ([] (list []))
  ([mark-or-marks] (list mark-or-marks {}))
  ([mark-or-marks {:keys [verbose]}]
   (doseq [[i [file entries]] (->> (query mark-or-marks)
                                   (group-by :file)
                                   (map-indexed vector))]
     (when (not= i 0)
       (newline))
     (printf "%s:\n" file)
     (doseq [[j entry] (->> entries
                            (sort-by (fn [{:keys [line column]}] [line column]))
                            (map-indexed vector))]
       (when (and verbose (= j 0))
         (println " --------------------------------"))
       (printf " * (%d:%d) [%s] %s\n"
               (:line entry)
               (:column entry)
               (name (:mark entry))
               (or (:comment entry) ""))
       (when verbose
         (print-code entry)
         (println " --------------------------------"))))))
