3. What does the following program do?

```
var a = 1;
{
  var a = a + 2;
  print a;
}
```

What did you expect it to do? Is it what you think it should do? What does analogous code in other languages you are familiar with do? What do you think users will expect this to do?

I expected it to print 3 and that's what it did. I can see an argument for trowing an exception instead. That's what Javascript does if you use `let` (not if you use `var`!). Users would also probably expect an exception. Frankly, for me this is ok.
