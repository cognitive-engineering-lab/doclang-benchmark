#lang scribble/base

@(require try-catch)

@(define x "Hello define")
@x

@(let ([x "Hello let"])
  @x)

@;{error: define: not allowed in an expression context}
@;{@bold{
  @(define y "Hello nested define")
  @y
}}

@bold{
  @(let ([x "Hello nested let"]) @x)
}
