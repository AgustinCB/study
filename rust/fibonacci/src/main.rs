fn main() {
    println!("Fibonacci: {}", fibonacci(1));
    println!("Fibonacci: {}", fibonacci(2));
    println!("Fibonacci: {}", fibonacci(3));
    println!("Fibonacci: {}", fibonacci(4));
    println!("Fibonacci: {}", fibonacci(5));
}

fn fibonacci(n: i32) -> i32 {
    let first_nums = [1, 1];
    if n == 1 {
        first_nums[0]
    } else if n == 2 {
        first_nums[1]
    } else {
        let mut prev: i32 = first_nums[1];
        let mut current: i32 = first_nums[1] + first_nums[0];
        for _ in 3..n {
            let new = current + prev;
            prev = current;
            current = new;
        }
        current
    }
}
