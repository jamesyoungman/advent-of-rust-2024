use std::str::FromStr;

pub fn parse_number<T>(s: &str) -> T
where
    T: FromStr,
{
    match s.trim().parse() {
        Ok(n) => n,
        Err(_) => {
            panic!("failed to parse '{s}' as a number");
        }
    }
}
