extern crate rand;

use rand::Rng;
use std::convert::TryInto;
// use std::{thread, time};

trait Runnable {
    fn step(&mut self);
    fn run(&mut self);
}

trait GameOfLifeRules {
    fn get_total_cells(&self) -> usize;
    fn get_cell_value(&self, cell_index: usize) -> u8;
    fn get_cell_neighours_count(&self, cell_index: usize) -> u8;
    fn set_cell_value(&mut self, cell_index: usize, cell_value: u8);

    fn apply_game_of_life_rules(&mut self) {
        for i in 0..self.get_total_cells() {
            let mut cell_value = self.get_cell_value(i);
            let cell_neighbours_count = self.get_cell_neighours_count(i);
            // println!("cell_index: {}, cell_value: {}, cell_neighbours_count: {}", i, cell_value, cell_neighbours_count);
            cell_value = if (cell_value == 0  && (cell_neighbours_count == 2 || cell_neighbours_count == 3)) || (cell_value == 0  && cell_neighbours_count == 3) {
                1
            } else {
                0
            };
            self.set_cell_value(i, cell_value)
        }
    }
}

struct World {
    cells: Vec<u8>,
    neighbours_count: Vec<u8>,
    height: usize,
    width: usize,
}

impl World {
    fn new(height: usize, width: usize) -> World {
        World {
            cells : vec![0; height * width],
            neighbours_count : vec![0; height * width],
            height : height,
            width : width,
        }
    }

    fn populate(&mut self, iterations: u32) {
        let mut rng = rand::thread_rng();
        for _i in 0..iterations {
            let index = rng.gen_range(0, self.cells.len());
            self.cells[index] = 1_u8;
            // println!("populated cells[{}]", index)
        }
        // println!("{}", self.to_string())
    }

    fn count_neighbours(&mut self) {
        for y in 0_i32..self.height.try_into().unwrap() {
            for x in 0_i32..self.width.try_into().unwrap() {
                let mut neighbours_count = 0_u8;
                neighbours_count += self.cells[modulus(y + 1, self.height) * self.width + modulus(x, self.width)];
                neighbours_count += self.cells[modulus(y + 1, self.height) * self.width + modulus(x + 1, self.width)];
                neighbours_count += self.cells[modulus(y, self.height) * self.width + modulus(x + 1, self.width)];
                neighbours_count += self.cells[modulus(y - 1, self.height) * self.width + modulus(x + 1, self.width)];
                neighbours_count += self.cells[modulus(y - 1, self.height) * self.width + modulus(x, self.width)];
                neighbours_count += self.cells[modulus(y - 1, self.height) * self.width + modulus(x - 1, self.width)];
                neighbours_count += self.cells[modulus(y, self.height) * self.width + modulus(x - 1, self.width)];
                neighbours_count += self.cells[modulus(y + 1, self.height) * self.width + modulus(x - 1, self.width)];
                self.neighbours_count[modulus(y, self.height) * self.width + modulus(x, self.width)] = neighbours_count;
            }
        }
    }
}

fn modulus(a: i32, b: usize) -> usize {
    let b: i32 = b.try_into().unwrap();
    (((a % b) + b) % b).try_into().unwrap()
}

impl std::string::ToString for World {
    fn to_string(&self) -> String {
        let mut display_raw = vec![0_u8; self.cells.len() + self.height];
        for line in 0..self.height {
            let cell_first = line * self.width;
            let cell_last = cell_first + self.width;
            let display_first = line * (self.width + 1);
            let display_last = display_first + self.width;
            display_raw[
                display_first..display_last
            ].copy_from_slice(&self.cells[
                cell_first..cell_last
            ]);
            display_raw[display_last] = 255_u8;
        }
        display_raw.iter().map(|&x| if x == 0_u8 {' '} else if x == 255_u8 {'\n'} else {'•'}).collect()
    }
}

impl GameOfLifeRules for World {
    fn get_total_cells(&self) -> usize { self.cells.len() }
    fn get_cell_value(&self, cell_index: usize) -> u8 { self.cells[cell_index] }
    fn get_cell_neighours_count(&self, cell_index: usize) -> u8 { self.neighbours_count[cell_index] }
    fn set_cell_value(&mut self, cell_index: usize, cell_value: u8) { self.cells[cell_index] = cell_value }
}

impl Runnable for World {
    fn step(&mut self) {
        self.count_neighbours();
        self.apply_game_of_life_rules();
        print!("{}{}", if true {"\x1b[0;0H"} else {""}, self.to_string());
    }

    fn run(&mut self) {
        print!("\x1b[0;0H\x1b[2J");
        while true {
            self.step();
            let delay = std::time::Duration::from_millis(150);
            std::thread::sleep(delay);
        }
    }
}

fn main() {
    let mut world = World::new(30, 60);
    world.populate(50);
    world.run();
    // world.step();
    // world.step();
}
