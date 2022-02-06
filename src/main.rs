use dialoguer::Input;
use std::{
    collections::HashMap,
    fs::OpenOptions,
    hash::{Hash, Hasher},
    io::Write,
    sync::Mutex,
};
use structopt::StructOpt;

#[derive(StructOpt)]
enum Command {
    Play,
    GenStats,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file_contents = std::fs::read_to_string("wordlist.txt")?;
    let all_words = file_contents.lines().collect::<Vec<_>>();
    let command = Command::from_args();

    match command {
        Command::Play => loop {
            let word = play_game(&all_words, |guess| {
                let outcome_str: String = Input::new()
                    .with_prompt(format!("Outcome of '{}'?", guess))
                    .interact_text()
                    .unwrap();
                Outcome::parse(&outcome_str)
            });
            println!("Word is: '{}'", word);
        },
        Command::GenStats => {
            let mut file = OpenOptions::new()
                .append(true)
                .create(true)
                .open("stats.txt")?;

            for secret in &all_words {
                let mut guesses = Vec::new();
                let word = play_game(&all_words, |guess| {
                    guesses.push(guess.to_string());
                    get_outcome(guess, secret)
                });

                guesses.push(word.to_string());
                assert_eq!(&word, secret);

                writeln!(&mut file, "{};{:?}", secret, guesses)?;
                eprintln!("{};{:?}", secret, guesses);
            }
            Ok(())
        }
    }
}

fn play_game<'s>(words: &[&'s str], mut get_outcome: impl FnMut(&str) -> Vec<Outcome>) -> &'s str {
    let mut possible_words = words.to_vec();

    while possible_words.len() > 1 {
        let guess = get_guess_word(&possible_words, words);
        let outcome = get_outcome(&guess);
        possible_words = filter_words(&possible_words, &guess, outcome);
    }
    possible_words.pop().unwrap()
}

lazy_static::lazy_static! {
    static ref START_CACHE: Mutex<HashMap<u64, String>> = Mutex::new(HashMap::new());
}

fn get_guess_word<'s>(possible: &[&str], guessable: &[&'s str]) -> String {
    if possible != guessable {
        return calc_guess_word(possible, guessable);
    }

    let mut hasher = std::collections::hash_map::DefaultHasher::default();
    guessable.hash(&mut hasher);
    let hash = hasher.finish();

    let mut cache_lock = START_CACHE.lock().unwrap();
    cache_lock
        .entry(hash)
        .or_insert_with(|| calc_guess_word(possible, guessable).to_string())
        .to_string()
}

fn calc_guess_word<'s>(possible_words: &[&str], guessable_words: &[&'s str]) -> String {
    guessable_words
        .iter()
        .min_by_key(|guessed_word| worst_bucket_size(guessed_word, possible_words))
        .unwrap()
        .to_string()
}

fn worst_bucket_size(guessed_word: &str, list: &[&str]) -> usize {
    let mut outcome_count = HashMap::new();

    for secret_word in list {
        *outcome_count
            .entry(get_outcome(guessed_word, secret_word))
            .or_insert(0) += 1;
    }

    outcome_count.into_values().max().unwrap()
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
