//  Copyright (c) 2026 Andrey Dubovik <andrei at dubovik dot eu>

// Rudimentary arrays with offsets

// Standard library
use std::ops::{Index, IndexMut};


pub struct Window<V> {
    pub lb: i64,
    pub ub: i64,
    pub data: Vec<V>,
}

impl<V: Default + Clone> Window<V> {
    pub fn new(lb: i64, ub: i64) -> Self {
        Window {
            lb: lb,
            ub: ub,
            // Initializing with V::default() is a latent error if some data is missing
            data: vec![V::default(); (ub - lb).try_into().expect("ub >= lb")],
        }
    }
}

impl<V> Index<i64> for Window<V> {
    type Output = V;

    fn index(&self, index: i64) -> &Self::Output {
        let index = usize::try_from(index - self.lb).expect("index >= lb");
        &self.data[index]
    }
}

impl<V> IndexMut<i64> for Window<V> {
    fn index_mut(&mut self, index: i64) -> &mut Self::Output {
        let index = usize::try_from(index - self.lb).expect("index >= lb");
        &mut self.data[index]
    }
}
