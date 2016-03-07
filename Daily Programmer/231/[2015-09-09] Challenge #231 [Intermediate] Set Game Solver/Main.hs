import Data.List
g[_,_]=1<0
g _=1>0----------x<-take 3<$>subsequences y
f y=unwords<$>[x|x<-subsequences y,all(g.nub)$transpose x]
main=interact$unlines.f.lines