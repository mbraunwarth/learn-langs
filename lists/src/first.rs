use std::mem;

pub struct List {
    head: Link,
}

impl List {
    // static method on the List type 
    
    // `new()` function - idiomatic for data type constructors in Rust
    //                    returning an 'idle' object of defined type 
    pub fn new() -> Self {
        List { head: Link::Empty }
    }

    // non-static methods operating on an object (`self`) of the List type
    
    pub fn push(&mut self, elem: i32) {
        let new_node = Box::new(Node { 
            elem: elem,
            // TODO here am i
            next: mem::replace(&self, List::new()),
        });

        self.head = Link::More(new_node);
    }

    pub fn pop(self) -> i32 {
        match self.head {
            Link::Empty => 0,
            Link::More(node) => node.elem,
        }
    }
}

enum Link {
    Empty,
    More(Box<Node>),
}

struct Node {
    elem: i32,
    next: List,
}
