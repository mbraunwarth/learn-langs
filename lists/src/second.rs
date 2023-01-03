use std::mem;

pub struct List<T> {
    head: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn push(&mut self, element: T) {
        let new_node = Box::new(Node {
            elem: element,
            next: mem::replace(&mut self.head, None),
        });

        self.head = Some(new_node)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }

    pub fn peek(&mut self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.elem)
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed_node) = cur_link {
            cur_link = boxed_node.next.take();
        }
    }
}

#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn pop_on_empty_list() {
        let mut list: List<i32> = List::new();
        assert_eq!(list.pop(), None);
    }

    #[test]
    fn pop_on_filled_list() {
        let mut list = List::new();
        list.push(1);
        assert_eq!(list.pop(), Some(1));
        assert_eq!(list.pop(), None);
    }

    #[test]
    fn pop_a_few() {
        let mut list = List::new();
        list.push(3);
        list.push(2);
        list.push(1);
        assert_eq!(list.pop(), Some(1));
        assert_eq!(list.pop(), Some(2));
        assert_eq!(list.pop(), Some(3));
        assert_eq!(list.pop(), None);
    }

    #[test]
    fn peek() {
        let mut list = List::<i32>::new();
        assert_eq!(list.peek(), None);
        list.push(1); list.push(2); list.push(3);

        assert_eq!(list.peek(), Some(&3));
        assert_eq!(list.pop(), Some(3));

        assert_eq!(list.peek_mut(), Some(&mut 2));

        // test mutability
        list.peek_mut().map(|value| *value = 42);
    }
}
