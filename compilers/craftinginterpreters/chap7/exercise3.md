3. This is highly tied to the language that implements lox, at least in my case.

In haskell, a division by zero would yeld the special value "Infinity." So if you do:

```
a :: Double
a = 2 / 0
```

The value of a would be `Infinity`. Due to this, that's also the value that Lox would return when doing the same operation. 

That seems fine to me. Other languages, such as Java, will decide to return an exception.

The problem with this solution is that when I implement the next interpret, I'd have to be complaint with this choise and make it also return `Infinity`, which might not be as trivial, specially if it's implemented in something like C.

Only because of that (to make my life easier) I'd change the current implementation to also return an exception. I otherwise think it's a better idea to return `Infinity`.
