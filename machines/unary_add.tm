input: '11111+11='
blank: '.'
start state: scanright
table:
  scanright:
    .: R
    1: R
    +: R
    =: {write: ., L: findlast}
  findlast:
    1: {write: ., L: removesign}
    +: {write: ., R: accept}
  removesign:
    1: L
    +: {write: 1, L: accept}
  accept: