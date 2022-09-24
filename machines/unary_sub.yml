input: '11111-11='
blank: '.'
start state: scanright
table:
  scanright:
    .: R
    1: R
    -: R
    =: {write: ., L: eraseone}
  eraseone:
    1: {write: =, L: subone}
    -: {write: ., L: accept}
  subone:
    1: L
    -: {L: skip}
  skip:
    .: L
    1: {write: ., R: scanright}

  accept: