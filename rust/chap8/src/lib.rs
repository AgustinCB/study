pub mod chap8 {
    use std::collections::HashMap;
    pub fn ex1(numbers: &[i32]) -> Vec<f64> {
        if numbers.len() == 0 {
            return vec![0.0, 0.0, 0.0];
        }
        let mut vector = numbers.to_vec();
        let mut scores = HashMap::new();
        for n in numbers.iter() {
            let count = scores.entry(n).or_insert(0);
            *count += 1;
        }
        vector.sort();
        let max = match scores.iter().max_by_key(|x| x.1) {
            Some((k, _)) => **k as f64,
            None => 0.0,
        };
        return vec![
            (numbers.iter().fold(0, |acc, &x| acc + x) as f64) / (numbers.len() as f64),
            vector[((vector.len() as i32) / 2) as usize] as f64,
            max
        ];
    }
    pub fn ex2(string: &str) -> String {
        let words = string.split_whitespace();
        let new_words: Vec<String> = words.map(to_pig_latin).collect();
        return new_words.join(" ");
    }
    fn to_pig_latin(word: &str) -> String {
        let vowels = ['a', 'e', 'i', 'o', 'u'];
        let mut chars = word.chars();
        let res: Option<(String, Vec<String>)> = chars.nth(0).map(|c| {
            return if vowels.contains(&c) {
                let mut parts: Vec<String> = chars.map(|c| c.to_string()).collect();
                parts.insert(0, c.to_string());
                (String::from("h"), parts)
            } else {
                (c.to_string(), chars.map(|c| c.to_string()).collect())
            }
        });
        return match res {
            Some((first, rest)) => rest.join("") + "-" + &first + "ay",
            None => String::from("")
        };
    }
}

#[cfg(test)]
mod tests {
    use super::chap8;

    #[test]
    fn it_works_1() {
        assert_eq!(chap8::ex1(&[][..]), [0.0, 0.0, 0.0]);
        assert_eq!(chap8::ex1(&[1][..]), [1.0, 1.0, 1.0]);
        assert_eq!(chap8::ex1(&[1, 2, 3, 2, 2][..]), [2.0, 2.0, 2.0]);
    }

    #[test]
    fn it_works_2() {
        assert_eq!(chap8::ex2(""), "");
        assert_eq!(chap8::ex2("hola"), "ola-hay");
        assert_eq!(chap8::ex2("alo"), "alo-hay");
        assert_eq!(chap8::ex2("hola alo"), "ola-hay alo-hay");
    }
}
