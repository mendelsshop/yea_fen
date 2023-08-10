//! psuedo random generator implentation (using xor shift)

// state of random numbers generator, intilalized with seed
static mut STATE: u32 = 1804289383;

fn get_random_number_u32() -> u32 {
    unsafe {
        STATE ^= STATE << 13;
        STATE ^= STATE >> 17;
        STATE ^= STATE << 5;

        STATE
    }
}

fn get_random_number_u64() -> u64 {
    let (n1, n2, n3, n4);

    // init random numbers slicing 16 bits from MS1B side
    n1 = get_random_number_u32() as u64 & 0xFFFF;
    n2 = get_random_number_u32() as u64 & 0xFFFF;
    n3 = get_random_number_u32() as u64 & 0xFFFF;
    n4 = get_random_number_u32() as u64 & 0xFFFF;

    // return random number
    return n1 | (n2 << 16) | (n3 << 32) | (n4 << 48);
}

pub fn generate_magic_number() -> u64 {
    get_random_number_u64() & get_random_number_u64() & get_random_number_u64()
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]
    use crate::{
        random::{generate_magic_number, get_random_number_u32, get_random_number_u64},
        BitBoard,
    };

    // #[test]
    // fn main() {
    //     println!(
    //         "{:?}",
    //         BitBoard::new((get_random_number_u32() as u64).into())
    //     );
    //     println!(
    //         "{:?}",
    //         BitBoard::new((get_random_number_u32() as u64 & 0xFFFF).into())
    //     ); // slice upper (from MSIB) 16 bits
    //     println!("{:?}", BitBoard::new(get_random_number_u64()));
    //     println!("{:?}", BitBoard::new(generate_magic_number()));
    // }
}
