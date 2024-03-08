use crate::Token;

pub trait Pattern<T>: Copy {
    fn accept(&self, t: T) -> bool;

    fn display(&self) -> String;
}

impl<'a> Pattern<Token<'a>> for Token<'a> {
    fn accept(&self, t: Token<'a>) -> bool {
        *self == t
    }

    fn display(&self) -> String {
        self.to_string()
    }
}

impl<T, F> Pattern<T> for F
where
    F: Fn(T) -> bool + Copy,
{
    fn accept(&self, t: T) -> bool {
        self(t)
    }

    fn display(&self) -> String {
        "<pattern function>".to_string()
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

    fn display(&self) -> String {
        format!(
            "[{}]",
            self.iter()
                .map(Pattern::display)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
