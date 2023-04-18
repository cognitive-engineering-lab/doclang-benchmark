@(define items (list "Milk" "Eggs" "Cheese"))

Today I am going shopping for:

@itemlist{
  @(for/list ([itm items])
    @item{@itm}
  )
}