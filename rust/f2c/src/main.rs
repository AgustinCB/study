fn main() {
    let f_temp = 70.0;
    println!("In celcius: {}", f2c(f_temp))
}

fn f2c(temp: f64) -> f64 {
    (temp - 32.0) / 1.8
}
