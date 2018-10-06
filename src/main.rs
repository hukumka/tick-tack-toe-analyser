//! # Tick-tac-toe game analyser
//!
//! https://en.wikipedia.org/wiki/Tic-tac-toe
//!
//! For each possible state of game (3x3) computes:
//!     + Existance of win strategy
//!     + Invetablility of loss
//! using minmax algorithm
//!
//! https://en.wikipedia.org/wiki/Minimax
//!
//! result stored in file "output.txt"

use lazy_static::lazy_static;

use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::BufWriter;


fn main() -> std::io::Result<()>{
    let res = AnalysisField::new().into_sorted_list();
    let mut buffer = BufWriter::new(File::create("output.txt")?);
    for (state, analysis, _) in res{
        use std::io::Write;
        write!(buffer, "{:?} = {:?}\n", state, analysis)?;
    }
    Ok(())
}


/// data structure to store results of analysis
struct AnalysisField{
    states: HashMap<State, Analysis>
}

impl AnalysisField{
    /// create new instance of AnalysisField and perform
    /// minmax search starting from empty board (player=X)
    fn new()->Self{
        let mut res = AnalysisField{states: HashMap::new()};
        let start_state = State::new();
        let first_player = Cell::Cross;
        res.depth_search(start_state, first_player);
        res
    }

    /// For given game state and player to make move determine:
    ///
    /// + result.will_win - current player can win no matter what
    ///     moves will his opponent do if he (current) plays correctly
    /// + result.will_lose - no matter which current player will choose
    ///     from now on, his opponent will win if he (opponent) plays 
    ///     correctly
    /// + result.player - current player
    ///
    /// And store this result to prevent reevaluating tree on further
    /// requests for the state, resulting in amortized O(1) time.
    fn depth_search(&mut self, state: State, player: Cell)->Analysis{
        if let Some(res) = self.states.get(&state){
            return (*res).clone();
        }
        let res = match state.result(){
            GameResult::Unfinished => {
                // if game is unfinished check what happend on next
                // leve of graph
                let will_win = self.next_level(&state, player).any(
                    |x| x.will_lose
                );
                let will_lose = self.next_level(&state, player).all(
                    |x| x.will_win
                );
                Analysis{will_win, will_lose, player}
            },
            GameResult::Win(winner) => {
                let will_win = winner == player;
                let will_lose = winner != player;
                Analysis{will_win, will_lose, player}
            },
            GameResult::Draw => {
                Analysis{will_win: false, will_lose: false, player}
            }
        };
        self.states.insert(state, res.clone());
        res
    }

    /// Returns iterator over results of ```depth_search``` for each
    /// state possible after player makes move.
    fn next_level<'a>(&'a mut self, state: &'a State, player: Cell)->impl Iterator<Item=Analysis> + 'a{
        state.moves().map(move |pos|{
            let mut new_state = state.clone();
            new_state.set(pos, player);
            self.depth_search(new_state, player.next())
        })
    }

    /// Transform data structure into list of triades
    /// + State - state of game for which result given
    /// + Analysis - result of ```depth_search``` for State
    /// + usize - amount of moves made in State
    ///
    /// list is sorted by amount of moves made in State
    fn into_sorted_list(self)->Vec<(State, Analysis, usize)>{
        let mut res: Vec<_> = self.states.into_iter()
            .map(|(s, a)| (s, a, s.move_count()))
            .collect();
        res.sort_by_key(|&(_, _, c)| c);
        res
    }
}



/// represent state of cell, also used for player
/// #TODO reason about
/// replacing Cell with Option<Player>
#[derive(Debug, PartialEq, Clone, Copy)]
enum Cell{
    Empty,
    Zero,
    Cross,
}

impl Cell{
    /// For given player identifier get identifier for next player
    /// to make a move
    fn next(self)->Cell{
        match self{
            Cell::Empty => Cell::Empty,
            Cell::Zero => Cell::Cross,
            Cell::Cross => Cell::Zero,
        }
    }
}


/// Represents result of game state
#[derive(Debug, PartialEq)]
enum GameResult{
    /// Game ended.
    ///
    /// Player specified in ```Cell``` have won.
    Win(Cell),
    /// Game ended.
    ///
    /// Its a draw.
    Draw,
    /// Game still going
    Unfinished,
}


/// represent state
///
/// each cell stored as 4 bits (36 bits in total)
/// (00 - Empty, 01 - Zero, 10 - Cross, 11 - invalid)
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct State(u64);

lazy_static!{
    /// just [0..3) x [0..3)
    static ref CELL_POS: Vec<(u32, u32)> = {
        let mut res = vec![];
        for x in 0..3{
            for y in 0..3{
                res.push((x, y));
            }
        }
        res
    };
}

impl fmt::Debug for State{
    fn fmt(&self, f: &mut fmt::Formatter)->fmt::Result{
        writeln!(f, "State{{");
        for x in 0..3{
            write!(f, "\t")?;
            for y in 0..3{
                let c = match self.get((x, y)){
                    Cell::Empty => '_',
                    Cell::Cross => 'X',
                    Cell::Zero => 'O',
                };
                write!(f, "{}", c)?;
            }
            writeln!(f, "")?;
        }
        write!(f, "}}")
    }
}

impl State{
    /// create state representing empty board
    fn new()->State{
        State(0)
    }

    /// get cell value of specified position
    ///
    /// panic if position is out of board
    pub fn get(&self, pos: (u32, u32))->Cell{
        match (self.0 >> Self::bit_offset(pos)) & 0b11{
            0b00 => Cell::Empty,
            0b01 => Cell::Zero,
            0b10 => Cell::Cross,
            other => {
                unreachable!("Invalid cell state = {}", other)
            }
        }
    }

    /// set cell value of specified position
    pub fn set(&mut self, pos: (u32, u32), value: Cell){
        let bit_offset = Self::bit_offset(pos);
        // set old value of cell to zero
        self.0 ^= ((self.0 >> bit_offset) & 0b11) << bit_offset;
        let bits = match value{
            Cell::Empty => 0b00,
            Cell::Zero => 0b01,
            Cell::Cross => 0b10,
        };
        self.0 |= bits << bit_offset;
    }

    /// get offset of 
    /// panic if position is out of board
    fn bit_offset(pos: (u32, u32))->u32{
        let (x, y) = pos;
        assert!(x < 3);
        assert!(y < 3);
        (x*3 + y) * 4
    }


    /// get iterator over all valid positions inside
    /// game board
    fn all_cells()->impl Iterator<Item=(u32, u32)>{
        CELL_POS.iter().cloned()
    }

    /// get iterator over all valid positions where move
    /// can be made
    fn moves<'a>(&'a self)->impl Iterator<Item=(u32, u32)> + 'a{
        State::all_cells().filter(move |x| self.get(*x) == Cell::Empty)
    }

    /// test game state and check if it ended and how
    fn result(&self)->GameResult{
        let rows: Vec<[_; 3]> = (0..3)
            .map(|x| [(x, 0), (x, 1), (x, 2)])
            .collect();
        let columns: Vec<[_; 3]> = (0..3)
            .map(|y| [(0, y), (1, y), (2, y)])
            .collect();
        let diags = vec![
            [(0, 0), (1, 1), (2, 2)],
            [(2, 0), (1, 1), (0, 2)],
        ];

        let mut total = rows;
        total.extend(columns);
        total.extend(diags);

        let has_c_z_e: Vec<_> = total.iter().map(|line|{
            (
                line.iter().any(|&x| self.get(x)==Cell::Cross),
                line.iter().any(|&x| self.get(x)==Cell::Zero),
                line.iter().any(|&x| self.get(x)==Cell::Empty),
            )
        }).collect();

        if has_c_z_e.iter().any(|&(c, z, e)| c & !z & !e){
            GameResult::Win(Cell::Cross)
        }else if has_c_z_e.iter().any(|&(c, z, e)| !c & z & !e){
            GameResult::Win(Cell::Zero)
        }else if has_c_z_e.iter().all(|&(c, z, _e)| c & z){
            GameResult::Draw
        }else{
            GameResult::Unfinished
        }
    }

    /// get amount of moves done so far
    fn move_count(&self)->usize{
        Self::all_cells().filter(|&x| self.get(x) != Cell::Empty).count()
    }
}


/// Struct representing result of analysis
#[derive(Debug, Clone)]
struct Analysis{
    /// current player to make a move (If game haven't ended yet)
    player: Cell,
    /// Shows existance of winning strategy for current player
    will_win: bool,
    /// Shows existance of winning strategy for opponent
    will_lose: bool,
}




#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_new(){
        assert_eq!(State::new().0, 0);
    }

    #[test]
    fn test_set_get(){
        let mut state = State::new();
        for pos in State::all_cells(){
            assert_eq!(state.get(pos), Cell::Empty);
        }

        let old_state = state.clone();
        state.set((0, 0), Cell::Cross);
        assert!(check_change(&old_state, &state, (0, 0)));
        assert_eq!(state.get((0, 0)), Cell::Cross);

        let old_state = state.clone();
        state.set((2, 1), Cell::Zero);
        assert!(check_change(&old_state, &state, (2, 1)));
        assert_eq!(state.get((2, 1)), Cell::Zero);
    }

    fn check_change(old_state: &State, new_state: &State, pos: (u32, u32))->bool{
        for p in State::all_cells(){
            let old = old_state.get(p);
            let new = new_state.get(p);
            if pos != p && old != new{
                return false;
            }
        }
        return true;
    }

    macro_rules! field{
        ($($e: ident,)*) => {
            {
                let v = vec![$(field!(#sym $e),)*];
                let mut state = State::new();
                for (v, pos) in v.into_iter().zip(State::all_cells()){
                    state.set(pos, v);
                }
                state
            }
        };
        (#sym x) => {
            Cell::Cross
        };
        (#sym o) => {
            Cell::Zero
        };
        (#sym e) => {
            Cell::Empty
        };
    }

    #[test]
    fn test_result(){
        let f = field!(
            e, e, e,
            e, e, e,
            e, e, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            e, e, e,
            e, x, e,
            e, e, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            e, e, e,
            e, x, o,
            e, e, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            e, x, e,
            e, x, o,
            e, e, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            e, x, e,
            e, x, o,
            e, o, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            x, x, e,
            e, x, o,
            e, o, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            x, x, o,
            e, x, o,
            e, o, e,
        );
        assert_eq!(f.result(), GameResult::Unfinished);

        let f = field!(
            x, x, o,
            e, x, o,
            e, o, x,
        );
        assert_eq!(f.result(), GameResult::Win(Cell::Cross));

        let f = field!(
            x, x, x,
            e, x, o,
            e, o, o,
        );
        assert_eq!(f.result(), GameResult::Win(Cell::Cross));

        let f = field!(
            x, x, o,
            x, x, o,
            e, o, o,
        );
        assert_eq!(f.result(), GameResult::Win(Cell::Zero));

        let f = field!(
            x, e, o,
            o, x, x,
            x, o, o,
        );
        assert_eq!(f.result(), GameResult::Draw);
    }

    #[test]
    fn test_analyser(){
        let f = field!(
            x, x, o,
            e, x, o,
            e, o, e,
        );
        let mut analyser = AnalysisField{states: HashMap::new()};
        assert!(analyser.depth_search(f, Cell::Cross).will_win);
    }
}
