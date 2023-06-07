#let x = 1

// 1
#x

#(
  let x = x + 1
)

// 2
#x

#[
  #let x = x + 1
]

// 2
#x