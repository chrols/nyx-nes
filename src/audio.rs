use sdl2::audio::{AudioCallback, AudioQueue, AudioSpecDesired};
use sdl2::AudioSubsystem;

use crate::apu::Apu;

struct SquareWave {
    duty_cycle: f32,
    phase_inc: f32,
    phase: f32,
    volume: f32,
}

impl AudioCallback for SquareWave {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        // Generate a square wave
        for x in out.iter_mut() {
            *x = if self.phase <= self.duty_cycle {
                self.volume
            } else {
                -self.volume
            };
            self.phase = (self.phase + self.phase_inc) % 1.0;
        }
    }
}

pub struct SdlApu {
    pulse_channel_1: SquareWave,
    pulse_channel_2: SquareWave,
    device1: AudioQueue<i16>,
    device2: AudioQueue<i16>,
}

fn gen_wave(bytes_to_write: i32, duty_cycle: f32, period: i32) -> Vec<i16> {
    // Generate a square wave
    let tone_volume = 1_000i16;
    let sample_count = bytes_to_write;
    let mut result = Vec::new();

    for x in 0..sample_count {
        result.push(if (x % period) <= (duty_cycle * period as f32) as i32 {
            tone_volume
        } else {
            -tone_volume
        });
    }
    result
}

impl SdlApu {
    pub fn new(audio_subsystem: AudioSubsystem) -> SdlApu {
        let desired_spec = AudioSpecDesired {
            freq: Some(44_100),
            channels: Some(1), // mono
            samples: None,     // default sample size
        };

        SdlApu {
            pulse_channel_1: SquareWave {
                duty_cycle: 0.5,
                phase_inc: 4400.0 / 44_100 as f32,
                phase: 0.0,
                volume: 0.25,
            },
            pulse_channel_2: SquareWave {
                duty_cycle: 0.5,
                phase_inc: 4400.0 / 44_100 as f32,
                phase: 0.0,
                volume: 0.25,
            },
            device1: audio_subsystem
                .open_queue::<i16, _>(None, &desired_spec)
                .unwrap(),
            device2: audio_subsystem
                .open_queue::<i16, _>(None, &desired_spec)
                .unwrap(),
        }
    }

    fn set_channel_1(&mut self, byte: u8) {
        // $4000 	DDlc.vvvv 	Pulse 1 Duty cycle, length counter halt, constant volume/envelope flag, and volume/envelope divider period
        self.pulse_channel_1.duty_cycle = match (byte >> 6) & 3 {
            0 => 0.125,
            1 => 0.25,
            2 => 0.5,
            3 => 0.75,
            _ => panic!("Invalid duty cycle"),
        };
        let duty_cycle = match (byte >> 6) & 3 {
            0 => 0.125,
            1 => 0.25,
            2 => 0.5,
            3 => 0.75,
            _ => 0.0,
        };
        self.pulse_channel_1.duty_cycle = 0f32;

        let wave = gen_wave(48_000 * 4, duty_cycle, 48_000 / 256);
        self.device1.queue(&wave);
        //self.device1.resume();
        let length_counter_halt = (byte >> 4) & 3;
    }

    fn set_channel_2(&mut self, byte: u8) {
        // $4000 	DDlc.vvvv 	Pulse 1 Duty cycle, length counter halt, constant volume/envelope flag, and volume/envelope divider period
        self.pulse_channel_2.duty_cycle = match (byte >> 6) & 3 {
            0 => 0.125,
            1 => 0.25,
            2 => 0.5,
            3 => 0.75,
            _ => panic!("Invalid duty cycle"),
        };
        self.pulse_channel_2.duty_cycle = 0f32;
        let duty_cycle = match (byte >> 6) & 3 {
            0 => 0.125,
            1 => 0.25,
            2 => 0.5,
            3 => 0.75,
            _ => 0.0,
        };
        let wave = gen_wave(48_000 * 4, duty_cycle, 48_000 / 256);
        self.device2.queue(&wave);
        //self.device2.resume();
        let length_counter_halt = (byte >> 4) & 3;
    }
}

impl Apu for SdlApu {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4015 => 0,
            _ => panic!("Attempt to read from APU: {:04X}", address),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x4000 => self.set_channel_1(byte),
            0x4001...0x4003 => (), //println!("{:04X} = {:04X}", address, byte),
            0x4004 => self.set_channel_2(byte),
            0x4005...0x4017 => (), //println!("{:04X} = {:04X}", address, byte),
            _ => panic!("Attempt to read from APU: {:04X}", address),
        }

        //self.device1.resume();
        //self.device2.resume();
    }

    fn cycle(&mut self) {}
}
