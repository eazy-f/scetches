struct ToHot;

impl Drop for ToHot {
    fn drop(&mut self) {
        println!("Already on the floor");
    }
}

fn main() {
    {
        let _x = ToHot;
    }
    println!("Bye-bye!")
}
