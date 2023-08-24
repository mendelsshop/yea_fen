use crate::{pop_bit, random::generate_magic_number, BitBoard, BISHOP_ATTACKS, ROOK_ATTACKS};

pub const NOT_H_FILE: BitBoard = BitBoard::new(9187201950435737471);
pub const NOT_A_FILE: BitBoard = BitBoard::new(18374403900871474942);
pub const NOT_HG_FILE: BitBoard = BitBoard::new(4557430888798830399);
pub const NOT_AB_FILE: BitBoard = BitBoard::new(18229723555195321596);

macro_rules! generate_array {
    ($size:literal, $fn:expr, $default:expr) => {{
        let mut ret = [$default; $size];

        let mut i = 0;
        while i < $size {
            ret[i] = $fn(i);
            i += 1;
        }
        ret
    }};
}
pub const PAWN_ATTACKS: [[BitBoard; 64]; 2] = {
    [
        generate_array!(64, generate_pawn_attack_white, BitBoard::new_empty()),
        generate_array!(64, generate_pawn_attack_black, BitBoard::new_empty()),
    ]
};

pub const KNIGHT_ATTACKS: [BitBoard; 64] =
    generate_array!(64, generate_knight_attacks, BitBoard::new_empty());

pub const KING_ATTACKS: [BitBoard; 64] =
    generate_array!(64, generate_king_attacks, BitBoard::new_empty());

pub const BISHOP_MASKS: [BitBoard; 64] =
    generate_array!(64, mask_bishop_attacks, BitBoard::new_empty());

pub const ROOK_MASKS: [BitBoard; 64] =
    generate_array!(64, mask_rook_attacks, BitBoard::new_empty());

// // TODO: maybe inline these constants as they are expansive to generate (although that would make the file big)
// pub const BISHOP_ATTACKS: [[BitBoard; 512]; 64] =
//     generate_array!(64, generate_bishop_attack, [BitBoard::new_empty(); 512]);

// // has to be static as opposed to const b/c it is so big it overflows stack see
// // https://github.com/rust-lang/rust/issues/82457
// // maybe bshop_attacks should be this way to
// pub static ROOK_ATTACKS: [[BitBoard; 4096]; 64] =
//     generate_array!(64, generate_rook_attack, [BitBoard::new_empty(); 4096]);

pub const BISHOP_RELEVANT_BITS: [u32; 64] = [
    6, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 6,
];

pub const ROOK_RELEVANT_BITS: [u32; 64] = [
    12, 11, 11, 11, 11, 11, 11, 12, 11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11, 12, 11, 11, 11, 11, 11, 11, 12,
];

/// use overflowing_* methods for this and [BISHOP_MAGIC_NUMBERS]
pub const ROOK_MAGIC_NUMBERS: [u64; 64] = [
    0x8a80104000800020,
    0x140002000100040,
    0x2801880a0017001,
    0x100081001000420,
    0x200020010080420,
    0x3001c0002010008,
    0x8480008002000100,
    0x2080088004402900,
    0x800098204000,
    0x2024401000200040,
    0x100802000801000,
    0x120800800801000,
    0x208808088000400,
    0x2802200800400,
    0x2200800100020080,
    0x801000060821100,
    0x80044006422000,
    0x100808020004000,
    0x12108a0010204200,
    0x140848010000802,
    0x481828014002800,
    0x8094004002004100,
    0x4010040010010802,
    0x20008806104,
    0x100400080208000,
    0x2040002120081000,
    0x21200680100081,
    0x20100080080080,
    0x2000a00200410,
    0x20080800400,
    0x80088400100102,
    0x80004600042881,
    0x4040008040800020,
    0x440003000200801,
    0x4200011004500,
    0x188020010100100,
    0x14800401802800,
    0x2080040080800200,
    0x124080204001001,
    0x200046502000484,
    0x480400080088020,
    0x1000422010034000,
    0x30200100110040,
    0x100021010009,
    0x2002080100110004,
    0x202008004008002,
    0x20020004010100,
    0x2048440040820001,
    0x101002200408200,
    0x40802000401080,
    0x4008142004410100,
    0x2060820c0120200,
    0x1001004080100,
    0x20c020080040080,
    0x2935610830022400,
    0x44440041009200,
    0x280001040802101,
    0x2100190040002085,
    0x80c0084100102001,
    0x4024081001000421,
    0x20030a0244872,
    0x12001008414402,
    0x2006104900a0804,
    0x1004081002402,
];

/// bishop magic numbers
pub const BISHOP_MAGIC_NUMBERS: [u64; 64] = [
    0x40040844404084,
    0x2004208a004208,
    0x10190041080202,
    0x108060845042010,
    0x581104180800210,
    0x2112080446200010,
    0x1080820820060210,
    0x3c0808410220200,
    0x4050404440404,
    0x21001420088,
    0x24d0080801082102,
    0x1020a0a020400,
    0x40308200402,
    0x4011002100800,
    0x401484104104005,
    0x801010402020200,
    0x400210c3880100,
    0x404022024108200,
    0x810018200204102,
    0x4002801a02003,
    0x85040820080400,
    0x810102c808880400,
    0xe900410884800,
    0x8002020480840102,
    0x220200865090201,
    0x2010100a02021202,
    0x152048408022401,
    0x20080002081110,
    0x4001001021004000,
    0x800040400a011002,
    0xe4004081011002,
    0x1c004001012080,
    0x8004200962a00220,
    0x8422100208500202,
    0x2000402200300c08,
    0x8646020080080080,
    0x80020a0200100808,
    0x2010004880111000,
    0x623000a080011400,
    0x42008c0340209202,
    0x209188240001000,
    0x400408a884001800,
    0x110400a6080400,
    0x1840060a44020800,
    0x90080104000041,
    0x201011000808101,
    0x1a2208080504f080,
    0x8012020600211212,
    0x500861011240000,
    0x180806108200800,
    0x4000020e01040044,
    0x300000261044000a,
    0x802241102020002,
    0x20906061210001,
    0x5a84841004010310,
    0x4010801011c04,
    0xa010109502200,
    0x4a02012000,
    0x500201010098b028,
    0x8040002811040900,
    0x28000010020204,
    0x6000020202d0240,
    0x8918844842082200,
    0x4010011029020020,
];

const fn generate_pawn_attack_white(position: usize) -> BitBoard {
    let mut attacks = BitBoard::new_empty();
    let piece = BitBoard::new_with_index(position as u64);

    // can think of right shifting by 7 (in our case) like turning on
    // the pawn on the next row foward 1
    // first check if moving piece
    if ((piece.board >> 7) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board >> 7
    }

    // same thing but going up row and then back 2
    if ((piece.board >> 9) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board >> 9
    }

    attacks
}

const fn generate_pawn_attack_black(position: usize) -> BitBoard {
    let mut attacks = BitBoard::new_empty();
    let piece = BitBoard::new_with_index(position as u64);

    if ((piece.board << 7) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board << 7
    }
    if ((piece.board << 9) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board << 9
    }

    attacks
}

const fn generate_knight_attacks(position: usize) -> BitBoard {
    let mut attacks = BitBoard::new_empty();
    let piece = BitBoard::new_with_index(position as u64);

    // left side
    if ((piece.board >> 17) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board >> 17
    };
    if ((piece.board >> 15) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board >> 15
    };
    if ((piece.board >> 10) & NOT_HG_FILE.board) != 0 {
        attacks.board |= piece.board >> 10
    };
    if ((piece.board >> 6) & NOT_AB_FILE.board) != 0 {
        attacks.board |= piece.board >> 6
    };

    // rights side
    if ((piece.board << 17) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board << 17
    };
    if ((piece.board << 15) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board << 15
    };
    if ((piece.board << 10) & NOT_AB_FILE.board) != 0 {
        attacks.board |= piece.board << 10
    };
    if ((piece.board << 6) & NOT_HG_FILE.board) != 0 {
        attacks.board |= piece.board << 6
    };
    attacks
}

const fn generate_king_attacks(position: usize) -> BitBoard {
    let mut attacks = BitBoard::new_empty();
    let piece = BitBoard::new_with_index(position as u64);

    // left side
    if (piece.board >> 8) != 0 {
        attacks.board |= piece.board >> 8
    };
    if ((piece.board >> 9) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board >> 9
    };
    if ((piece.board >> 7) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board >> 7
    };
    if ((piece.board >> 1) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board >> 1
    };

    // rights side
    if (piece.board << 8) != 0 {
        attacks.board |= piece.board << 8
    };
    if ((piece.board << 9) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board << 9
    };
    if ((piece.board << 7) & NOT_H_FILE.board) != 0 {
        attacks.board |= piece.board << 7
    };
    if ((piece.board << 1) & NOT_A_FILE.board) != 0 {
        attacks.board |= piece.board << 1
    };

    attacks
}

const fn mask_bishop_attacks(position: usize) -> BitBoard {
    let mut attacks = BitBoard::new_empty();

    let target_rank: i32 = position as i32 / 8;
    let target_file: i32 = position as i32 % 8;
    let (mut file, mut rank): (i32, i32);
    // mark bishop occupancy squares
    // (r = tr + 1, f = tf + 1; r <= 6 && f <= 6; r++, f++) attacks |= (1 << (r * 8 + f));
    (file, rank) = (target_file + 1, target_rank + 1);
    while rank <= 6 && file <= 6 {
        attacks.board |= 1 << (rank * 8 + file);

        rank += 1;
        file += 1;
    }
    // for (r = tr - 1, f = tf + 1; r >= 1 && f <= 6; r--, f++) attacks |= (1 << (r * 8 + f));
    (file, rank) = (target_file + 1, target_rank - 1);
    while rank >= 1 && file <= 6 {
        attacks.board |= 1 << (rank * 8 + file);

        rank -= 1;
        file += 1;
    }

    // for (r = tr + 1, f = tf - 1; r <= 6 && f >= 1; r++, f--) attacks |= (1 << (r * 8 + f));
    (file, rank) = (target_file - 1, target_rank + 1);
    while rank <= 6 && file >= 1 {
        attacks.board |= 1 << (rank * 8 + file);

        rank += 1;
        file -= 1;
    }
    // for (r = tr - 1, f = tf - 1; r >= 1 && f >= 1; r--, f--) attacks |= (1 << (r * 8 + f));
    (file, rank) = (target_file - 1, target_rank - 1);
    while rank >= 1 && file >= 1 {
        attacks.board |= 1 << (rank * 8 + file);

        rank -= 1;
        file -= 1;
    }

    attacks
}

const fn mask_rook_attacks(position: usize) -> BitBoard {
    let mut attacks = BitBoard::new_empty();

    let target_rank: i32 = position as i32 / 8;
    let target_file: i32 = position as i32 % 8;
    let (mut file, mut rank): (i32, i32);
    // mark rook occupancy squares

    // for (r = tr + 1; r <= 6; r++) attacks |= (1 << (r * 8 + tf));
    rank = target_rank + 1;
    while rank <= 6 {
        attacks.board |= 1 << (rank * 8 + target_file);

        rank += 1;
    }

    // for (r = tr - 1; r >= 1; r--) attacks |= (1 << (r * 8 + tf));
    rank = target_rank - 1;
    while rank >= 1 {
        attacks.board |= 1 << (rank * 8 + target_file);

        rank -= 1;
    }

    // for (f = tf + 1; f <= 6; f++) attacks |= (1 << (tr * 8 + f));
    file = target_file + 1;
    while file <= 6 {
        attacks.board |= 1 << (target_rank * 8 + file);

        file += 1;
    }

    // for (f = tf - 1; f >= 1; f--) attacks |= (1 << (tr * 8 + f));
    file = target_file - 1;
    while file >= 1 {
        attacks.board |= 1 << (target_rank * 8 + file);

        file -= 1;
    }

    attacks
}

const fn bishop_attacks_on_the_fly(position: usize, block: BitBoard) -> BitBoard {
    let mut attacks = BitBoard::new_empty();

    let target_rank: i32 = position as i32 / 8;
    let target_file: i32 = position as i32 % 8;
    let (mut file, mut rank): (i32, i32);
    // we first set attack even if we hit another piece as it only matters for the next portion of the ray
    // if we first checked there would be no moves generated with kills
    (file, rank) = (target_file + 1, target_rank + 1);
    while rank <= 7 && file <= 7 {
        attacks.board |= 1 << (rank * 8 + file);
        if ((1 << (rank * 8 + file)) & block.board) != 0 {
            break;
        }

        rank += 1;
        file += 1;
    }

    (file, rank) = (target_file + 1, target_rank - 1);
    while rank >= 0 && file <= 7 {
        attacks.board |= 1 << (rank * 8 + file);
        if ((1 << (rank * 8 + file)) & block.board) != 0 {
            break;
        }

        rank -= 1;
        file += 1;
    }

    (file, rank) = (target_file - 1, target_rank + 1);
    while rank <= 7 && file >= 0 {
        attacks.board |= 1 << (rank * 8 + file);
        if ((1 << (rank * 8 + file)) & block.board) != 0 {
            break;
        }

        rank += 1;
        file -= 1;
    }

    (file, rank) = (target_file - 1, target_rank - 1);
    while rank >= 0 && file >= 0 {
        attacks.board |= 1 << (rank * 8 + file);
        if ((1 << (rank * 8 + file)) & block.board) != 0 {
            break;
        }

        rank -= 1;
        file -= 1;
    }

    attacks
}

const fn rook_attacks_on_the_fly(position: usize, block: BitBoard) -> BitBoard {
    let mut attacks = BitBoard::new_empty();

    let target_rank: i32 = position as i32 / 8;
    let target_file: i32 = position as i32 % 8;
    let (mut file, mut rank): (i32, i32);

    rank = target_rank + 1;
    while rank <= 7 {
        attacks.board |= 1 << (rank * 8 + target_file);
        if ((1 << (rank * 8 + target_file)) & block.board) != 0 {
            break;
        }

        rank += 1;
    }

    rank = target_rank - 1;
    while rank >= 0 {
        attacks.board |= 1 << (rank * 8 + target_file);
        if ((1 << (rank * 8 + target_file)) & block.board) != 0 {
            break;
        }

        rank -= 1;
    }

    file = target_file + 1;
    while file <= 7 {
        attacks.board |= 1 << (target_rank * 8 + file);
        if ((1 << (target_rank * 8 + file)) & block.board) != 0 {
            break;
        }

        file += 1;
    }

    file = target_file - 1;
    while file >= 0 {
        attacks.board |= 1 << (target_rank * 8 + file);
        if ((1 << (target_rank * 8 + file)) & block.board) != 0 {
            break;
        }

        file -= 1;
    }

    attacks
}

const fn set_occupancy(index: usize, bits_in_mask: u32, mut attack_mask: BitBoard) -> BitBoard {
    let mut occupancy = BitBoard::new_empty();

    {
        let mut count = 0;
        while count < bits_in_mask {
            let square = attack_mask.least_significant_first_bit_index() as usize;
            pop_bit!(attack_mask, square);

            if (index & (1 << count)) != 0 {
                occupancy.board |= 1 << square
            }

            count += 1;
        }
    }

    occupancy
}

fn find_magic_number<const N: usize>(
    square: usize,
    relevant_bits: u32,
    is_bishop: bool,
) -> BitBoard {
    let mut occupancies: [BitBoard; N] = [BitBoard::default(); N];

    let mut attacks: [BitBoard; N] = [BitBoard::default(); N];

    let mut used_attaks: [BitBoard; N];

    let attack_mask = if is_bishop {
        mask_bishop_attacks(square)
    } else {
        mask_rook_attacks(square)
    };

    let occupancy_indicies = 1 << relevant_bits;

    for index in 0..occupancy_indicies {
        occupancies[index] = set_occupancy(index, relevant_bits, attack_mask);

        attacks[index] = if is_bishop {
            bishop_attacks_on_the_fly(square, occupancies[index])
        } else {
            rook_attacks_on_the_fly(square, occupancies[index])
        };
    }
    // magic number
    for _ in 0..usize::MAX {
        used_attaks = [BitBoard::default(); N];

        let magic_number = generate_magic_number();

        // skip "bad" magic number
        if BitBoard::new((attack_mask.board.overflowing_mul(magic_number)).0 & 0xFF00000000000000)
            .count_bits()
            < 6
        {
            continue;
        }

        // teest magic index
        let mut fail = false;
        for index in 0..occupancy_indicies {
            if fail {
                break;
            }

            let magic_index = (occupancies[index].board.overflowing_mul(magic_number).0) as usize
                >> (64 - relevant_bits);

            if used_attaks[magic_index].board == 0 {
                used_attaks[magic_index] = attacks[index];
            } else if used_attaks[magic_index] != attacks[index] {
                fail = true;
            }
        }
        if !fail {
            return BitBoard::new(magic_number);
        }
    }
    println!("magic numbers fails!!!");
    BitBoard::default()
}

/// The reason we don't use this to initilize magic number arrays is a) its expansive b) those magic number arrays couldn't be constant b/c random number generation is generally not constany
#[allow(unused)]
fn print_magic_numbers() {
    // NOTE: magic numbers will be diferrent if you call random `get_random_number_u32` or the like before
    println!("--------rooks--------");
    for square in 0..64 {
        let magic_number = find_magic_number::<4096>(square, ROOK_RELEVANT_BITS[square], false);
        println!("0x{:x}", magic_number.board)
    }
    println!("-------bishops-------");
    for square in 0..64 {
        let magic_number = find_magic_number::<4096>(square, BISHOP_RELEVANT_BITS[square], true);
        println!("0x{:x}", magic_number.board)
    }
}

const fn generate_bishop_attack(square: usize) -> [BitBoard; 512] {
    let attack_mask = BISHOP_MASKS[square];

    // init relevant occupancy bit count
    let relevant_bits_count = attack_mask.count_bits();

    // init occupancy indicies
    let occupancy_indicies = 1 << relevant_bits_count;
    let mut bishop_attacks: [BitBoard; 512] = [BitBoard::new_empty(); 512];
    let mut index = 0;
    while index < occupancy_indicies {
        // init current occupancy variation
        let occupancy = set_occupancy(index, relevant_bits_count, attack_mask);

        // init magic index
        let magic_index = ((occupancy
            .board
            .overflowing_mul(BISHOP_MAGIC_NUMBERS[square])
            .0)
            >> (64 - BISHOP_RELEVANT_BITS[square])) as usize;

        // init bishop attacks
        bishop_attacks[magic_index] = bishop_attacks_on_the_fly(square, occupancy);

        index += 1;
    }
    bishop_attacks
}

const fn generate_rook_attack(square: usize) -> [BitBoard; 4096] {
    let attack_mask = ROOK_MASKS[square];

    // init relevant occupancy bit count
    let relevant_bits_count = attack_mask.count_bits();

    // init occupancy indicies
    let occupancy_indicies = 1 << relevant_bits_count;

    let mut rook_attacks: [BitBoard; 4096] = [BitBoard::new_empty(); 4096];
    let mut index = 0;
    while index < occupancy_indicies {
        // init current occupancy variation
        let occupancy = set_occupancy(index, relevant_bits_count, attack_mask);

        // init magic index
        let magic_index: usize = ((occupancy
            .board
            .overflowing_mul(ROOK_MAGIC_NUMBERS[square])
            .0)
            >> (64 - ROOK_RELEVANT_BITS[square])) as usize;

        // init bishop attacks
        rook_attacks[magic_index] = rook_attacks_on_the_fly(square, occupancy);

        index += 1;
    }
    rook_attacks
}

pub const fn get_bishop_attacks(square: usize, mut occupancy: BitBoard) -> BitBoard {
    occupancy.board &= BISHOP_MASKS[square].board;
    occupancy.board = occupancy
        .board
        .overflowing_mul(BISHOP_MAGIC_NUMBERS[square])
        .0;
    occupancy.board >>= 64 - BISHOP_RELEVANT_BITS[square];
    BISHOP_ATTACKS[square][occupancy.board as usize]
}
pub fn get_rook_attacks(square: usize, mut occupancy: BitBoard) -> BitBoard {
    occupancy.board &= ROOK_MASKS[square].board;
    occupancy.board = occupancy
        .board
        .overflowing_mul(ROOK_MAGIC_NUMBERS[square])
        .0;
    occupancy.board >>= 64 - ROOK_RELEVANT_BITS[square];
    ROOK_ATTACKS[square][occupancy.board as usize]
}

pub fn get_queen_attacks(square: usize, occupancy: BitBoard) -> BitBoard {
    get_bishop_attacks(square, occupancy) | get_rook_attacks(square, occupancy)
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]
    use std::str::FromStr;

    use crate::{
        moves::{
            get_bishop_attacks,
            get_queen_attacks,
            // BISHOP_ATTACKS,
            get_rook_attacks,
            mask_rook_attacks,
            rook_attacks_on_the_fly,
        },
        BitBoard, Board,
    };

    use super::{print_magic_numbers, set_occupancy};

    #[test]
    fn print_rook_with_blocker() {
        let blockers = Board::from_str("r3k2r/p1ppqpb1/bn2pnp1/3PN3/3pP3/1p12p2/PpPBBpPP/R3K2R")
            .unwrap()
            .black_pawns;
        let moves = rook_attacks_on_the_fly(44, blockers);
        println!("{:?}", moves)
    }

    #[test]
    fn occupancy() {
        let attack_mask = mask_rook_attacks(56);

        let occupancy = set_occupancy(4095, attack_mask.count_bits(), attack_mask);

        println!("{:?}", occupancy)
    }

    #[test]
    /// TAKES TIME DONT RUN (if you do use, run in release)
    fn print_magic_number() {
        print_magic_numbers();
    }

    #[test]
    fn bishop_with_blocker() {
        let mut bb = BitBoard::default();
        bb.set_index(26);
        bb.set_index(53);
        bb.set_index(14);
        bb.set_index(53);
        bb.set_index(49);
        println!("{bb:?}");

        let attacks = get_bishop_attacks(35, bb);

        println!("{:?}", attacks);
    }

    #[test]
    fn rook_with_blocker() {
        let mut bb = BitBoard::default();
        bb.set_index(26);
        bb.set_index(53);
        bb.set_index(14);
        bb.set_index(53);
        bb.set_index(49);
        bb.set_index(30);
        bb.set_index(52);
        bb.set_index(12);
        println!("{bb:?}");

        let attacks = get_rook_attacks(28, bb);

        println!("{:?}", attacks);
    }

    #[test]
    fn queen_with_blocker() {
        let mut bb = BitBoard::default();
        bb.set_index(26);
        bb.set_index(53);
        bb.set_index(14);
        bb.set_index(53);
        bb.set_index(49);
        bb.set_index(30);
        bb.set_index(52);
        bb.set_index(12);
        println!("{bb:?}");

        let attacks = get_queen_attacks(28, bb);

        println!("{:?}", attacks);
    }
}
