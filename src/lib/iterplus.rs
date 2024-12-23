pub fn sum_result<T, Q, E>(total: T, current: Result<Q, E>) -> Result<T, E>
where
    T: std::ops::Add<Output = T>,
    Q: Into<T>,
{
    current.map(|x: Q| total + x.into())
}

pub fn sum_result_refs<T, Q, E>(total: T, current: &Result<Q, E>) -> Result<T, E>
where
    T: std::ops::Add<Output = T>,
    Q: Into<T> + Copy,
    E: Copy,
{
    match current {
        Ok(c) => Ok(total + (*c).into()),
        Err(e) => Err(*e),
    }
}

#[test]
fn test_sum_result() {
    let input: Vec<Result<i32, ()>> = vec![Ok(1), Ok(2), Ok(800)];

    let total = input.iter().try_fold(-1_i32, sum_result_refs);
    assert!(matches!(total, Ok(802)), "{total:?}");

    let total = input.into_iter().try_fold(-1_i32, sum_result);
    assert!(matches!(total, Ok(802)), "{total:?}");
}

#[test]
fn test_sum_result_empty() {
    let input: Vec<Result<i32, ()>> = vec![];
    let total = input.iter().try_fold(0, sum_result_refs);
    assert!(matches!(total, Ok(0)), "{total:?}");

    let total = input.into_iter().try_fold(0, sum_result);
    assert!(matches!(total, Ok(0)), "{total:?}");
}

#[test]
fn test_sum_result_widen() {
    let input: Vec<Result<i32, ()>> = vec![];

    let total = input.iter().try_fold(0_i64, sum_result_refs);
    assert!(matches!(total, Ok(0_i64)), "{total:?}");

    let total = input.into_iter().try_fold(0_i64, sum_result);
    assert!(matches!(total, Ok(0_i64)), "{total:?}");
}

#[test]
fn test_sum_result_of_conversion() {
    let input: Vec<&str> = vec!["1", "2"];
    let total: Result<i64, _> = input
        .iter()
        .map(|s| s.parse::<i64>())
        .try_fold(0_i64, sum_result);
    assert!(matches!(total, Ok(3_i64)), "{total:?}");
}

#[test]
fn test_sum_result_propagate_error() {
    let input: Vec<&str> = vec!["1", "2", "not-a-number"];
    let total: Result<i64, _> = input
        .iter()
        .map(|s| s.parse::<i64>())
        .try_fold(0_i64, sum_result);
    assert!(matches!(total, Err(_)), "{total:?}");
}

/// Returns all pairs generatable from the input slice.
pub fn all_pairs<T>(v: &[T]) -> impl Iterator<Item = (&T, &T)> + use<'_, T> {
    v.iter()
        .enumerate()
        .flat_map(|(i, right)| v[0..i].iter().map(move |left| (left, right)))
}

#[test]
fn test_all_pairs_nocopy() {
    #[derive(Eq, PartialEq, Debug)]
    struct NoCopy {}

    assert_eq!(all_pairs::<NoCopy>(&[]).collect::<Vec<_>>(), Vec::new());
}

#[test]
fn test_all_pairs() {
    assert_eq!(all_pairs::<char>(&[]).collect::<Vec<_>>(), Vec::new());

    assert_eq!(
        all_pairs(&['a', 'b']).collect::<Vec<_>>(),
        vec![(&'a', &'b')]
    );
    assert_eq!(
        all_pairs(&['a', 'b', 'c']).collect::<Vec<_>>(),
        vec![(&'a', &'b'), (&'a', &'c'), (&'b', &'c')]
    );
}
