use dialoguer::Input;
use indicatif::{ParallelProgressIterator, ProgressBar};
use rayon::iter::*;
use std::{
    collections::{HashMap, HashSet},
    fs::OpenOptions,
    hash::{Hash, Hasher},
    io::Write,
    sync::Mutex,
};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Options {
    #[structopt(long, default_value = "bonus_guessable.txt")]
    guessable_words: String,

    #[structopt(long, default_value = "wordlist.txt")]
    possible_words: String,

    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt)]
enum Command {
    Play,

    PlayAll {
        #[structopt(long, default_value = "all_plays.txt")]
        out: String,
    },
}

lazy_static::lazy_static! {
    static ref START_CACHE: Mutex<lru::LruCache<u64, String>> = Mutex::new(lru::LruCache::new(1_000));
    static ref MAX_POSSIBLE: Mutex<Option<usize>> = Mutex::new(None);

    static ref PROGRESS_LOCK: Mutex<()> = Mutex::new(());
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let options = Options::from_args();

    let possible_contents = std::fs::read_to_string(&options.possible_words)?;
    let possible_words = possible_contents.lines().collect::<Vec<_>>();

    let guessable_contents = std::fs::read_to_string(&options.guessable_words)?;
    let mut guessable_words = guessable_contents.lines().collect::<HashSet<_>>();
    guessable_words.extend(&possible_words);
    let guessable_words = guessable_words.into_iter().collect::<Vec<_>>();

    match options.command {
        Command::Play => loop {
            let word = play_game(&possible_words, &guessable_words, |guess| {
                let outcome_str: String = Input::new()
                    .with_prompt(format!("Outcome of '{}'?", guess))
                    .interact_text()
                    .unwrap();
                Outcome::parse(&outcome_str)
            });
            println!("Word is: '{}'", word);
        },
        Command::PlayAll { out } => {
            let file = Mutex::new(OpenOptions::new().append(true).create(true).open(&out)?);

            possible_words
                .par_iter()
                .progress_with(if let Ok(_) = PROGRESS_LOCK.try_lock() {
                    ProgressBar::new(possible_words.len() as u64)
                } else {
                    ProgressBar::hidden()
                })
                .map(|secret| {
                    let mut guesses = Vec::new();
                    let word = play_game(&possible_words, &guessable_words, |guess| {
                        guesses.push(guess.to_string());
                        get_outcome(guess, secret)
                    });

                    guesses.push(word.to_string());
                    assert_eq!(&word, secret);
                    (secret, guesses)
                })
                .try_for_each(|(secret, guesses)| {
                    writeln!(&mut file.lock().unwrap(), "{};{:?}", secret, guesses)
                })?;
            Ok(())
        }
    }
}

fn play_game<'s>(
    possible_words: &[&'s str],
    guessable_words: &[&str],
    mut get_outcome: impl FnMut(&str) -> Vec<Outcome>,
) -> &'s str {
    let mut possible_words = possible_words.to_vec();

    loop {
        match &possible_words[..] {
            &[] => unreachable!("no possible words"),
            &[s] => return s,
            _ => {
                let guess = get_guess_word(&possible_words, guessable_words);
                let outcome = get_outcome(&guess);
                possible_words = filter_words(&possible_words, &guess, outcome);
            }
        }
    }
}

fn get_guess_word<'s>(possible: &[&str], guessable: &[&'s str]) -> String {
    let mut max_possible_lock = MAX_POSSIBLE.lock().unwrap();
    let use_cache = match *max_possible_lock {
        None => {
            *max_possible_lock = Some(possible.len());
            true
        }
        Some(l) if l <= possible.len() => {
            *max_possible_lock = Some(possible.len());
            true
        }
        Some(_) => false,
    };
    drop(max_possible_lock);

    if use_cache {
        let mut hasher = std::collections::hash_map::DefaultHasher::default();

        possible.hash(&mut hasher);
        guessable.hash(&mut hasher);
        let hash = hasher.finish();

        match START_CACHE.lock().unwrap().get(&hash) {
            Some(v) => return v.clone(),
            None => {}
        }

        let word = calc_guess_word(possible, guessable);
        START_CACHE.lock().unwrap().put(hash, word.to_string());
        word.to_string()
    } else {
        calc_guess_word(possible, guessable).to_string()
    }
}

fn calc_guess_word(possible_words: &[&str], guessable_words: &[&str]) -> String {
    guessable_words
        .par_iter()
        .progress_with(if let Ok(_) = PROGRESS_LOCK.try_lock() {
            ProgressBar::new(guessable_words.len() as u64)
        } else {
            ProgressBar::hidden()
        })
        .min_by_key(|guessed_word| {
            float_ord::FloatOrd(expected_score(guessed_word, possible_words))
        })
        .unwrap()
        .to_string()
}

fn expected_score(guessed: &str, possible: &[&str]) -> f64 {
    let possible_prob = 1.0 / possible.len() as f64;
    let possible_entropy = (-possible_prob * possible_prob.log2()) * possible.len() as f64;

    let correct_prob = if possible.contains(&guessed) {
        possible_prob
    } else {
        0.
    };

    correct_prob
        + (1.0 - correct_prob)
            * expected_score_from_entropy(possible_entropy - bucket_entropy(guessed, possible))
}

fn expected_score_from_entropy(entropy: f64) -> f64 {
    1. + 0.56 * (entropy + 1.).ln() + 0.1 * entropy
}

fn bucket_entropy(guessed_word: &str, list: &[&str]) -> f64 {
    let mut outcome_count = HashMap::new();

    for secret_word in list {
        *outcome_count
            .entry(get_outcome(guessed_word, secret_word))
            .or_insert(0) += 1;
    }

    outcome_count
        .into_values()
        .map(|count| {
            let prob = count as f64 / list.len() as f64;
            -prob * prob.log2()
        })
        .sum()
}

fn get_outcome(guessed_word: &str, secret_word: &str) -> Vec<Outcome> {
    assert_eq!(guessed_word.len(), secret_word.len());

    let mut unmatched_chars = secret_word.chars().collect::<Vec<_>>();
    let remove_unmatched = |chars: &mut Vec<char>, c: char| {
        let i = chars.iter().position(|inner_c| c == *inner_c).unwrap();
        chars.remove(i);
    };

    let mut result = vec![Outcome::None; guessed_word.len()];

    for (i, (guessed, secret)) in guessed_word.chars().zip(secret_word.chars()).enumerate() {
        if guessed == secret {
            remove_unmatched(&mut unmatched_chars, guessed);
            result[i] = Outcome::Exact;
        }
    }

    for (i, guessed) in guessed_word.chars().enumerate() {
        if result[i] != Outcome::None {
            continue;
        }

        if unmatched_chars.contains(&guessed) {
            remove_unmatched(&mut unmatched_chars, guessed);
            result[i] = Outcome::Misplaced;
        }
    }

    result
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Outcome {
    Exact,
    Misplaced,
    None,
}

impl Outcome {
    fn parse(s: &str) -> Vec<Outcome> {
        s.chars()
            .map(|c| match c {
                'e' => Outcome::Exact,
                'm' => Outcome::Misplaced,
                'n' => Outcome::None,
                _ => panic!("Unexpected char {}", c),
            })
            .collect()
    }
}

fn filter_words<'s>(list: &[&'s str], guessed_word: &str, outcome: Vec<Outcome>) -> Vec<&'s str> {
    list.iter()
        .filter(|secret_word| get_outcome(guessed_word, secret_word) == outcome)
        .map(|&s| s)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    mod get_outcome {
        use super::*;

        #[test]
        fn alloy_aloft() {
            assert_eq!(
                get_outcome("alloy", "aloft"),
                vec![
                    Outcome::Exact,
                    Outcome::Exact,
                    Outcome::None,
                    Outcome::Misplaced,
                    Outcome::None,
                ]
            );
        }

        #[test]
        fn teett_tatet() {
            assert_eq!(
                get_outcome("teett", "tatet"),
                vec![
                    Outcome::Exact,
                    Outcome::Misplaced,
                    Outcome::None,
                    Outcome::Misplaced,
                    Outcome::Exact,
                ]
            );
        }

        #[test]
        fn aback_beach() {
            assert_eq!(
                get_outcome("aback", "beach"),
                vec![
                    Outcome::None,
                    Outcome::Misplaced,
                    Outcome::Exact,
                    Outcome::Exact,
                    Outcome::None,
                ]
            );
        }
    }
}
