w '/'=' '
w c=c
h(a:s:k:e:l)=h$a*e+k*s:s*e:l
h l@[n,d]=show.(`div`gcd n d)<$>l
v[n,d]=n++'/':d
main=interact$v.h.map read.tail.words.map w