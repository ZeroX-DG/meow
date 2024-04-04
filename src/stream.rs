use std::collections::VecDeque;

pub struct ParsingStream<'a, T: PartialEq + Clone> {
    buffer: VecDeque<T>,
    input: &'a mut dyn Iterator<Item=T>,
    stream_end_item: T
}

impl<'a, T: PartialEq + Clone> ParsingStream<'a, T> {
    pub fn new(input: &'a mut dyn Iterator<Item=T>, stream_end_item: T) -> Self {
        Self {
            buffer: VecDeque::new(),
            input,
            stream_end_item
        }
    }

    pub fn next(&mut self) -> T {
        if !self.buffer.is_empty() {
            let consumed = self.buffer.pop_front();
            return consumed.unwrap();
        }
        self.input.next().unwrap_or(self.stream_end_item.clone())
    }

    pub fn consume_until<F: Fn(&T) -> bool>(&mut self, cond: F) {
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