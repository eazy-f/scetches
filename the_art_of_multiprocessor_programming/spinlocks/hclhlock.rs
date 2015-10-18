use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::Arc;
use std::thread;
use std::cell::UnsafeCell;

enum QNodeState {FREE, WAITING, ABORTED}

struct QNode {
    state: QNodeState
}

/* unsafe impl Send for QNode {} */

/*type QNodePtr = Option<Box<QNode>>;*/
type QNodePtr = Option<QNode>;

struct HCLHLock {
    null_ptr: AtomicPtr<QNodePtr>,
    local_queue: AtomicPtr<QNodePtr>,
    global_queue: AtomicPtr<QNodePtr>,
}

impl HCLHLock {
    fn new() -> HCLHLock {
        let null_ptr = UnsafeCell::new(None);
        HCLHLock{
            null_ptr: AtomicPtr::new(null_ptr.get()),
            local_queue: AtomicPtr::new(null_ptr.get()),
            global_queue: AtomicPtr::new(null_ptr.get()),
        }
    }

    fn lock(&self, node: *mut QNodePtr) -> bool {
        let queue = &self.local_queue;
        let null = self.null_ptr.load(Ordering::Relaxed);
        let prev = queue.compare_and_swap(null, node, Ordering::Relaxed);
        prev == null
    }
}

fn main() {
    let lock = Arc::new(HCLHLock::new());
    let mut children = Vec::new();

    for i in 0..3 {
        let thread_lock = lock.clone();
        let handle = thread::spawn(move || {
            let mut node = Some(QNode{state: QNodeState::FREE});
            if thread_lock.lock(&mut node) {
                println!("{}: I won", i);
            }
        });
        children.push(handle);
    }

    /* how to do this with map and a closure? */
    for thread in children {
        thread.join().unwrap();
    }
}
