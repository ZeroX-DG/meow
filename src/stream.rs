use std::collections::VecDeque;

pub struct ParsingStream<'a, T> {
    buffer: VecDeque<T>,
    input: &'a mut dyn Iterator<Item=T>
}

impl<'a, T> ParsingStream<'a, T> {
    pub fn new(input: &'a mut dyn Iterator<Item=T>) -> Self {
        Self {
            buffer: VecDeque::new(),
            input,
        }
    }

    pub fn next(&mut self) -> Option<T> {
        if !self.buffer.is_empty() {
            let consumed = self.buffer.pop_front();
            return consumed;
        }
        self.input.next()
    }

    pub fn consume_until<F: Fn(&T) -> bool>(&mut self, cond: F) {
        while let Some(item) = self.next() {
            if cond(&item) {
                break;
            }
        }
    }

    pub fn peek(&mut self, amount: usize) -> &VecDeque<T> {
        let buffer_size = self.buffer.len();

        if amount < buffer_size {
            return &self.buffer;
        }

        let consume_amount = amount - buffer_size;
        for _ in 0..consume_amount {
            if let Some(item) = self.input.next() {
                self.buffer.push_back(item);
            }
        }
        &self.buffer
    }
}