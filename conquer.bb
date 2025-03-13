#!/usr/bin/env bb
;; -*- mode: clojure -*-

(ns conquer
  "A command line utility to generate text files from (comb) templates.
  In the computer game Master of Orion II before terraforming a
  planet, you had to conquer it."
  (:require [shell-smith.core :as smith]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [babashka.fs :as fs]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [comb.template :as comb]
            [cheshire.core :as json]))

(def ^:dynamic *config*)

(def usage "
conquer

Usage:
  conquer [-t=<template>] [-u] [-p=file-or-directory]
  conquer -h | --help
  conquer -v | --version
Options:
  -h --help                 Show this screen.
  -p --path=<path>          Path to operate on [default: .]
  -t --template=<template>  Override the template file
  -u --update               Creates or updates files.
  -v --version              Show version.

----

conquer is a command line utility to generate text files from (comb)
templates. Takes a yml or csv files or as input. For all input files
apply the therein mentioned templates or the template given via
config. Each input file can hold a single entry or a list of entries
to be processed. The resulting file will have its basename from the
input file and its extension from the template (the first template in
case of a list).")

(defn stubborn-eval* [template bindings]
  (fn []
    (try
      (comb/eval template bindings)
      (catch clojure.lang.ExceptionInfo e
        (if-let [[_ missing] (re-matches #"Could not resolve symbol: ([\w-]+)" (ex-message e))]
          ;; in case it is just a missing var, add missing var to
          ;; bindings and re-wrap in a function for next trampoline
          ;; iteration
          (stubborn-eval* template (assoc bindings (keyword missing) ""))
          ;; re-throw instead
          (throw e))))))

(defn stubborn-eval [template bindings]
  (trampoline (stubborn-eval* template bindings)))

(defn conquer-entry [path {:keys [template] :as entry}]
  (let [filename (str path "/../" (or (:template *config*) template))]
    (-> filename
        fs/canonicalize
        str
        slurp
        (stubborn-eval entry))))

(defn update-file [path content]
  (when-not (and (fs/regular-file? path)
                 (= (slurp path) content))
    (spit path content)
    (println "Updated" path)))

(defmulti read-file (fn [file] (-> file (str/split #"\.") last keyword)))

(defmethod read-file :yml [file]
  (-> file slurp yaml/parse-string))

(defmethod read-file :csv [file]
  (with-open [reader (io/reader file)]
    (let [[headers & rows] (csv/read-csv reader)
          headers (map keyword headers)]
      (mapv (partial zipmap headers) rows))))

(defn- ext
  ([path] (ext path 0))
  ([path n]
   (-> path (str/split #"\.") reverse (nth n))))

(defn conquer-file [file]
  (let [entries (-> file read-file vector flatten)
        iext (ext file)
        oext (-> (:template *config*)
                 (or (-> entries first :template))
                 (ext 1))
        content (->> entries
                     (map (partial conquer-entry file))
                     (str/join "\n"))]
    (if (:update *config*)
      (update-file (str/replace file (re-pattern (str iext "$")) oext) content)
      (println content))))

(defn conquer-files [files]
  (run! conquer-file files))

(defn conquer-directory [file-or-directory]
  ;; debug
  (println "ls " (str file-or-directory "/**.{yml,yaml,csv}"))
  (prn (fs/glob file-or-directory "**.{yml,yaml,csv}"))
  (as-> file-or-directory %
    (fs/glob % "**.{yml,yaml,csv}")
    (map str %)
    (conquer-files %)))

;; --------------------------------------------------------------------------------

;; shorthands for usage in templates

(def json json/generate-string)

;; --------------------------------------------------------------------------------

(defn -main [& args]
  (binding [*config* (smith/config usage)]
    (let [{:keys [path]} *config*]
      (cond
        (:help *config*)
        (println usage)

        (:version *config*)
        (println "conquer 0.1.1 (LULW)")

        :else
        (cond
          (nil? path)
          (conquer-directory ".")

          (fs/directory? path)
          (conquer-directory path)

          (fs/regular-file? path)
          (conquer-files [path]))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
