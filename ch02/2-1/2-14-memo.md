
B1 -> a +- b%, 0<=b<=1, [a*(1-b), a*(1+b)]
B2 -> c +- d%, 0<=d<=1, [c*(1-d), c*(1+d)]


B1 * B2 = [ac(1-(b+d)), ac*(1+(b+d))]
center:  ac
width: (b+d)
percent: (b+d)/ac


B1 + B2 = [a*(1-b)+c*(1-d), a*(1+b)+c*(1+d)]


B1 * B2 / (B1 + B2) =  ac(1-(b+d))*ac*(1+(b+d))/a*(1+b)+c*(1+d) + a*(1-b)+c*(1-d)

1/B1 = 1/{a*(1+b)}, 1/{a*(1-b)}
1/B2 = 1/{c*(1+d)}, 1/{c*(1-d)}

1/B1 + 1/B2 = 1/{a*(1+b)} + 1/{c*(1+d)}, 1/{a*(1-b)} + 1/{c*(1-d)}

1/ (1/B1 + 1/B2) = 1/{1/{a*(1-b)} + 1/{c*(1-d)}, 1/{1/{a*(1+b)} + 1/{c*(1+d)}}
= 
;;

par1とpar2で積(mul-interval)の回数が異なる。(par1は2回, par2は3回, div-intervalは内部でmul-intervalを1回呼んでいる。

```
(mul-interval
 (div-interval (make-interval 1 1)
               (make-interval 1 2))
 (make-interval 1 2))

>> (0.5 . 2.0)
```
