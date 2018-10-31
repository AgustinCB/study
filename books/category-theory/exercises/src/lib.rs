// Chapter 1

// Exercise 1
fn identity<T>(a: T) -> T {
    a
}

// Exercise 2
fn composition<'f, F, G, A, B, C>(f: F, g: G) -> Box<Fn(A) -> C + 'f>
    where F: Fn(B) -> C + 'f,
          G: Fn(A) -> B + 'f {
    Box::new(move |a| f(g(a)))
}

/*
 * Exercise 4:
 *   Is the world-wide web a category in any sense? Are links morphisms?
 * 
 * Yes! Links are morphisms. Link to self is the identity function. With that associativity
 * working perfectly.
 *
 * Exercise 5:
 *   Is Facebook a category, with people as objects and friendships as morphisms?
 *
 * Nope. You can't be friend with yourself. No identity function.
 *
 * Exercise 6:
 *   When is a directed graph a category?
 *
 * When every object has an arrow to self.
 */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_should_return_self() {
        assert_eq!(2, identity(2));
        assert_eq!("asd", identity("asd"));
    }

    #[test]
    fn composition_should_execute_one_after_the_other() {
        let h = |a| a + 2;
        let g = |a| a * 3;
        let f = |a| a - 3;
        let c = |a| h(composition(g, f)(a));
        let c1 = |a| composition(h, g)(f(a));
        let c2 = |a| h(g(f(a)));
        assert_eq!(c(1), c1(1));
        assert_eq!(c1(1), c2(1));
    }

    #[test]
    fn composition_should_handle_different_types() {
        let h = composition(|a: Option<usize>| a.unwrap_or(0), |b| Some(b));
        assert_eq!(h(4), 4);
    }

    #[test]
    fn composition_respects_identity() {
        let h = composition(identity, |b| b*2);
        assert_eq!(h(4), 8);
    }
}
