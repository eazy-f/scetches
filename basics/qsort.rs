use std::cmp::Ord;
use std::str::FromStr;
use std::rand;

fn main () {
    let mut arr = Vec::<i64>::new();

    for arg in std::env::args() {
        match FromStr::from_str(&arg) {
            Ok(n) => arr.push(n),
            _     => ()
        }
    }

    qsort(&mut arr);
    println!("{:?}", arr);
}

fn qsort<T>(arr : &mut[T]) where T: Ord + Copy + Clone {
    /* qsort_copy(arr); */
    qsort_inplace(arr);
    /* arr.sort() */;
}

fn qsort_copy<T>(arr : &mut[T]) where T: Ord + Copy + Clone {
    let mut copy = arr.to_vec();
    qsort2(arr, &mut copy);
}

fn qsort_inplace<T>(arr0 : &mut[T]) where T: Ord + Copy + Clone {
    let mut arr = arr0;
    let mut size;

    /* use loop as emulation of tail recursion */
    loop {
        size = arr.len();
        if size < 2 {
            break;
        }

        let x = arr;
        let split = qsort_inplace_partition(x);
        if split > (size / 2) {
            {
                qsort_inplace(&mut x[split..size]);
            }
            arr = &mut x[0..split];
        } else {
            {
                qsort_inplace(&mut x[0..split]);
            }
            arr = &mut x[split..size];
        };
    }
}

fn qsort_inplace_partition<T>(arr : &mut[T]) -> usize
    where T: Ord + Copy + Clone
{
    let size = arr.len();
    let rand : usize = rand::random() % size;
    let pivot = arr[rand];
    let (mut left, mut right) = (0, size - 1);
    let mut split = 0;

    arr[rand] = arr[0];
    arr[0] = pivot;

    while left < right  {
        if arr[left] >= pivot {
            let mut found = false;
            while left < right && !found {
                if arr[right] < pivot {
                    let swap = arr[left];
                    arr[left] = arr[right];
                    arr[right] = swap;
                    split = left + 1;
                    found = true;
                }
                right -= 1
            }
        }
        left += 1
    }

    if split == 0 {
        1
    } else {
        split
    }
}


fn qsort2<T>(left0 : &mut[T], right : &mut[T])
    where T: Ord + Copy
{
    let size  = left0.len();
    if size == 0 {
        return
    }

    let pivot = left0[0];

    /* this block is needed to limit scope of `left`
       and use borrowed `left0` afterwards */
    let copied = {
        let left  = &mut left0[1..size];
        let x = copy_less(pivot, left, right);
        right[x] = pivot;
        copy_great(pivot, left, &mut right[(x+1)..size]);
        x
    };
    left0[copied] = pivot;

    if size == 2 {
        copy_filter(|_| {true}, right, left0);
    } else if size > 2 {
        let start = copied + 1;
        qsort2(&mut right[0..copied], &mut left0[0..copied]);
        qsort2(&mut right[start..size], &mut left0[start..size]);
    }
}

fn copy_less<T>(pivot : T, from : &mut[T], to : &mut[T]) -> usize
    where T: Ord + Copy
{
    copy_filter(|x| {x < pivot}, from, to)
}

fn copy_great<T>(pivot : T, from : &mut[T], to : &mut[T]) -> usize
    where T: Ord + Copy
{
    copy_filter(|x| {x >= pivot}, from, to)
}

fn copy_filter<T, F>(filter : F, from : &mut[T], to : &mut[T]) -> usize
    where T: Ord + Copy, F: Fn(T) -> bool
{
    from.iter().fold(0, |acc, val| {
        if filter(*val) {
            to[acc] = *val;
            acc+1
        } else {
            acc
        }
    })
}
