use std::sync::atomic::{AtomicInt, Ordering};

fn main() {
  let reg = AtomicInt::new(0);
  /* calls cmpxchg somewhere */
  reg.compare_and_swap(0, 1, Ordering::Relaxed);
}
