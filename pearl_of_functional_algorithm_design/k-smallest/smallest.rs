use std::fmt::Display;
use std::cmp;
use std::str::FromStr;
use std::iter::FromIterator;

fn main() {
    let args = Vec::<String>::from_iter(std::env::args());
    let k: usize = FromStr::from_str(&args[1]).unwrap();
    let a_size: usize = FromStr::from_str(&args[2]).unwrap();
    let b_size: usize = FromStr::from_str(&args[3]).unwrap();
    let mut a = Vec::<i64>::with_capacity(a_size);
    let mut b = Vec::<i64>::with_capacity(b_size);
    for i in 0..a_size {
        a.push((i as i64) * 2);
    }
    for i in 0..b_size {
        b.push((i as i64)*2 + 1);
    }

    a.sort();
    b.sort();

    println!("k-smallest: {}", smallest(k, &a[..], &b[..]));
}

fn smallest<T>(k: usize, left: &[T], right: &[T]) -> T
    where T: Ord + Copy + Display {
    let (mut small, big) =
        if left.len() < right.len() {
            (left, right)
        } else {
            (right, left)
        };
    if small.len() > k {
        small = &small[0..(k - 1)]
    }
    let mut big_used = k - small.len();
    let small_saved = small;
    let mut iterations = 0;

    loop {
        if big.len() == 0 {
            return small[small.len() - 1];
        }
        if big_used == k {
            return big[big_used - 1];
        }
        if small.len() == 0 || big_used == big.len() {
            println!("finish: {}, iterations: {}", big_used, iterations);
            return cmp::max(big[big_used - 1], small_saved[k - big_used - 1]);
        }
        iterations = iterations + 1;
        let half_candidate = small.len() / 2;
        let big_half = big_used + (small.len() - half_candidate) - 1;
        let half = if big_half > big.len() {
            half_candidate + (big_half - big.len())
        }
        else {
            half_candidate
        };
        println!("{} {} {}", small.len(), half, big_used);
        println!("{} {}", small[half],
                 big[big_used + (small.len() - half) - 1]);
        if small[half] < big[big_used + (small.len() - half) - 1] {
            small = &small[(half+1)..];
        }
        else {
            big_used = big_used + (small.len() - half);
            small = &small[0..half];
        }
    }
}
