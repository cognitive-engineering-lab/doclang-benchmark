The naive use of `@for/list` in `bad.html` desugars into:

```racket
(((fold-var)
  (let-values (((fold-var)
                fold-var))
    (let-values (((fold-var)
                  (let-values ()
                    (#%app
                      cons
                      (let-values ()
                        '"<tr><td>"
                        (#%app
                        car
                        c)
                        '", "
                        (#%app
                        cdr
                        c)
                        '"</td></tr>")
                      fold-var))))
      (#%app
        values
        fold-var)))))
```

Which produces the unexpected output `</td></tr></td></tr>` for a two-element list input.