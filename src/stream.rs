use std::collections::VecDeque;

macro_rules! peek {
    ($stream:ident) => {
        $stream.peek(1)
    };
    ($stream:ident, $n:expr) => {
        $stream.peek($n)
    };
}

pub(crate) use peek;

pub struct ParsingStream<'a, T: PartialEq + Clone> {
    input: &'a mut dyn Iterator<Item = T>,
    stream_end_item: T,
    buffer: VecDeque<T>,
}

impl<'a, T: PartialEq + Clone> ParsingStream<'a, T> {
    pub fn new(input: &'a mut dyn Iterator<Item = T>, stream_end_item: T) -> Self {
        let first_item = input.next().unwrap_or(stream_end_item.clone());
        Self {
            input,
            stream_end_item,
            buffer: vec![first_item].into(),
        }
    }

    pub fn next(&mut self) -> T {
        let result = self.buffer.pop_front().unwrap();

        if self.buffer.is_empty() {
            self.buffer
                .push_back(self.input.next().unwrap_or(self.stream_end_item.clone()));
        }

        result
    }

    pub fn consume_until<F: FnMut(&T) -> bool>(&mut self, mut cond: F) {
        loop {
            let item = self.next();
            if cond(&item) {
                break;
            }

            if item == self.stream_end_item {
                break;
            }
        }
    }

    pub fn peek(&mut self, n: usize) -> T {
        if n == 0 {
            unreachable!("Can't peek i + 0 item");
        }

        if n <= self.buffer.len() {
            return self.buffer.get(n - 1).unwrap().clone();
        }

        for _ in 0..n {
            let item = self.input.next().unwrap_or(self.stream_end_item.clone());
            self.buffer.push_back(item);
        }

        return self.buffer.get(n - 1).unwrap().clone();
    }
}

