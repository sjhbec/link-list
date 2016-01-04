# link-list
This is linked list for storing polymorphic data block written in FORTRAN.
Here, we consider each link to have the following format

link:
| Length of Key | No of row of the block(m) | No of columns of the block(n) | Type of Data(p) | Starting Location of values | --- data block values |
  --- 1 Byte ---  ---- 1 Byte ------           ----- 1 Byte  ------           -- 1 Byte --      ---- 1 Byte ----              --- m X n X p Bytes----

Some of the routines are obtained from other open sources. The detail documentation and the references are under preparation.



