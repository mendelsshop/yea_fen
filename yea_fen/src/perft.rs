// perft.c

// #include "defs.h"
// #include "stdio.h"

use std::{str::FromStr, time::Instant};

use crate::{chess_engines::simple_ai, GameState};

// long leafNodes;
static mut LEAFNODES: usize = 0;

// void Perft(int depth, S_BOARD *pos) {

//     ASSERT(CheckBoard(pos));

// 	if(depth == 0) {
//         leafNodes++;
//         return;
//     }

//     S_MOVELIST list[1];
//     GenerateAllMoves(pos,list);

//     int MoveNum = 0;
// 	for(MoveNum = 0; MoveNum < list->count; ++MoveNum) {

//         if ( !MakeMove(pos,list->moves[MoveNum].move))  {
//             continue;
//         }
//         Perft(depth - 1, pos);
//         TakeMove(pos);
//     }

//     return;
// }

fn perft(gs: &mut GameState, depth: usize) {
    if depth == 0 {
        unsafe {
            LEAFNODES += 1;
        }
        return;
    }
    let moves = simple_ai::generate_all_moves(gs);
    for r#move in moves {
        if !simple_ai::do_move(gs, r#move) {
            continue;
        }
        perft(gs, depth - 1);
        gs.undo_move();
    }
}

// void PerftTest(int depth, S_BOARD *pos) {

//     ASSERT(CheckBoard(pos));

// 	PrintBoard(pos);
// 	printf("\nStarting Test To Depth:%d\n",depth);
// 	leafNodes = 0;
// 	int start = GetTimeMs();
//     S_MOVELIST list[1];
//     GenerateAllMoves(pos,list);

//     int move;
//     int MoveNum = 0;
// 	for(MoveNum = 0; MoveNum < list->count; ++MoveNum) {
//         move = list->moves[MoveNum].move;
//         if ( !MakeMove(pos,move))  {
//             continue;
//         }
//         long cumnodes = leafNodes;
//         Perft(depth - 1, pos);
//         TakeMove(pos);
//         long oldnodes = leafNodes - cumnodes;
//         printf("move %d : %s : %ld\n",MoveNum+1,PrMove(move),oldnodes);
//     }

// 	printf("\nTest Complete : %ld nodes visited in %dms\n",leafNodes,GetTimeMs() - start);

//     return;
// }

fn perft_test(gs: &mut GameState, depth: usize) {
    unsafe {
        LEAFNODES = 0;
    }
    let now = Instant::now();
    let moves = simple_ai::generate_all_moves(gs);
    for r#move in moves.iter().enumerate() {
        if !simple_ai::do_move(gs, *r#move.1) {
            continue;
        }
        let cum_nodes = unsafe { LEAFNODES };
        perft(gs, depth - 1);
        gs.undo_move();
        let old_nodes = unsafe { LEAFNODES } - cum_nodes;
        println!("move {} : {} : {old_nodes}", r#move.0, r#move.1)
    }
    println!(
        "\nTest Complete : {} nodes visited in {}s",
        unsafe { LEAFNODES },
        now.elapsed().as_secs()
    )
}

#[test]
fn perft_start_3() {
    let mut gs = GameState::new();
    perft_test(&mut gs, 4)
}
