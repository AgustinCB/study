2. 
How do other languages you know handle local variables that refer to the same name in their initializer, like:

```
var a = "outer";
{
var a = a;
}
```

Is it a runtime error? Compile error? Allowed? Do they treat global variables differently? Do you agree with their choices? Justify your answer.

Rust would allow you to do that, by creating a shadow of the new variable that can reference the previous one. The logic behind it is to allow you to do stuff like this:

```
let port: String = std::env::args()[0];
let port: u32 = u32::parse_str_radix(port, 10);
```

It makes some sense, but it can become confusing if abused.

I'm not sure whether I like it or not. I certainly understand why it is needed, specially in rust which sometimes forces you to bind expressions to variables.
