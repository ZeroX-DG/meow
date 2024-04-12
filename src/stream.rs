pub struct ParsingStream<'a, T: PartialEq + Clone> {
    next_item: T,
    input: &'a mut dyn Iterator<Item = T>,
    stream_end_item: T,
}

impl<'a, T: PartialEq + Clone> ParsingStream<'a, T> {
    pub fn new(input: &'a mut dyn Iterator<Item = T>, stream_end_item: T) -> Self {
        Self {
            next_item: input.next().unwrap_or(stream_end_item.clone()),
            input,
            stream_end_item,
        }
    }

    pub fn next(&mut self) -> T {
        let result = self.next_item.clone();
        self.next_item = self.input.next().unwrap_or(self.stream_end_item.clone());

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

    pub fn peek(&mut self) -> T {
        self.next_item.clone()
    }
}
