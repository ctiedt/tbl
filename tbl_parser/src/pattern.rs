use crate::Token;

pub trait Pattern<T> {
    fn accept(&self, t: T) -> bool;
}

impl<'a> Pattern<Token<'a>> for Token<'a> {
    fn accept(&self, t: Token<'a>) -> bool {
        *self == t
    }
}

impl<T, F> Pattern<T> for F
where
    F: Fn(T) -> bool,
{
    fn accept(&self, t: T) -> bool {
        self(t)
    }
}

impl<T, P> Pattern<T> for &[P]
where
    P: Pattern<T>,
    T: Copy,
{
    fn accept(&self, t: T) -> bool {
        self.iter().any(|p| p.accept(t))
    }
}
