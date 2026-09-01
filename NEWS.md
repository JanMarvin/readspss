# readspss 0.20

## Bug fixes

* `write.por()` no longer fails on negative numbers. The sign was
  dropped before the number was encoded, which produced an unreadable
  value and on some platforms crashed the session (#36). Files also
  carry two more digits of precision than before.

* `read.por()` no longer loses precision on values with long mantissas.
  The digits were accumulated into a single number that grew past what a
  double can hold exactly, so the low bits were lost before the value
  was assembled. `1e6` used to read back slightly wrong; it is now
  exact.

Portable files still do not round-trip bit-for-bit: about 75% of
arbitrary doubles survive a write and read unchanged, the rest differ in
the last decimal digit. That is far below any practical
significance, but exact comparisons such as `identical()` will fail on
some values.

These changes have been checked against readspss itself. They have not been
checked against SPSS.
