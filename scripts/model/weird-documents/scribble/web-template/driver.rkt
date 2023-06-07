#lang racket

(require web-server/templates)

(let ([clients (list (cons "Young" "Brigham") (cons "Smith" "Joseph"))])
  (println "bad.html:")
  (println (include-template "bad.html"))
  (println "good.html")
  (println (include-template "good.html")))