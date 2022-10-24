use bitflags::bitflags;
use getopts::Options;
use std::fmt;
use std::fs::File;
use std::io::{Read, Write};
use std::io::{Seek, SeekFrom};

const MBR_BOOT_SIG: [u8; 2] = [0x55, 0xAA];

enum PartitionType {
    Fat16,
}

struct Partition {
    typ: PartitionType,
    /// Starting sector number
    start: u32,
    /// Size in sectors
    size: u32,
}

#[derive(Copy, Clone)]
struct FatDateTime {
    date: u16,
    time: u16,
    /// Optional millisecond value, 0 if not present.
    /// Stored as counts of 10 milliseconds (1/100 of a second)
    millis: u8,
}

impl FatDateTime {
    fn from_date(date: u16) -> Self {
        Self {
            date,
            time: 0,
            millis: 0,
        }
    }

    fn from_date_time(date: u16, time: u16) -> Self {
        Self {
            date,
            time,
            millis: 0,
        }
    }

    fn from_date_time_fraction(date: u16, time: u16, millis: u8) -> Self {
        Self { date, time, millis }
    }

    /// Year, in range 1980-2107
    fn year(&self) -> u16 {
        1980 + ((self.date >> 9) & 0b1111111)
    }
    /// Month of the year, in range 1-12
    fn month(&self) -> u8 {
        (self.date >> 5) as u8 & 0b1111
    }
    /// Day of the month, in range 1-31
    fn day(&self) -> u8 {
        self.date as u8 & 0b11111
    }
    /// Hours, in range 0-23
    fn hours(&self) -> u8 {
        (self.time >> 11) as u8 & 0b11111
    }
    /// Minutes, in range 0-59
    fn minutes(&self) -> u8 {
        (self.time >> 5) as u8 & 0b111111
    }
    /// Seconds, in range 0-59.
    /// This does account for the tenths value
    fn seconds(&self) -> u8 {
        (self.time as u8 & 0b11111) * 2 + self.millis / 100
    }
    /// Milliseconds
    fn millis(&self) -> u16 {
        (self.millis % 100) as u16 * 10
    }
}

impl fmt::Display for FatDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}",
            self.year(),
            self.month(),
            self.day(),
            self.hours(),
            self.minutes(),
            self.seconds(),
            self.millis()
        )
    }
}

bitflags! {
    struct FatAttributes : u8 {
        const READ_ONLY = 0x01;
        const HIDDEN    = 0x02;
        const SYSTEM    = 0x04;
        const VOLUME_ID = 0x08;
        const DIRECTORY = 0x10;
        const ARCHIVE   = 0x20;

        const LONG_NAME = Self::READ_ONLY.bits | Self::HIDDEN.bits | Self::SYSTEM.bits | Self::VOLUME_ID.bits;
    }
}

fn main() {
    let mut opts = Options::new();
    opts.optopt("p", "partition", "Partition number", "PARTITION");

    let args = std::env::args().collect::<Vec<_>>();
    let matches = opts.parse(&args[1..]).unwrap();

    let partition: Option<u8> = matches.opt_str("p").map(|p| p.parse().unwrap());
    if matches.free.is_empty() {
        eprintln!(
            "usage: {} <DISKFILE> [FILE_TO_EXTRACT] [-p PARTITION]",
            args[0]
        );
        std::process::exit(1);
    }
    let disk = &matches.free[0];
    let ex_file_path = matches
        .free
        .get(1)
        .map(|e| e.split('/').collect::<Vec<_>>());

    let mut disk = File::open(disk).expect("Could not open file");

    let mut mbr = [0u8; 512];
    disk.read_exact(&mut mbr)
        .expect("Expected at least 512 bytes for MBR");

    assert_eq!(
        &mbr[0x1FE..=0x1FF],
        &MBR_BOOT_SIG,
        "No MBR boot signature found"
    );

    let partitions = mbr[0x1BE..0x1BE + 4 * 0x10]
        .chunks(0x10)
        .enumerate()
        .filter_map(|(ii, partition)| {
            let _bootable = partition[0x0] & 0x80 != 0;
            let _first_chs: [u8; 3] = partition[0x1..0x1 + 3].try_into().unwrap();
            let part_type = partition[0x4];
            let _last_chs: [u8; 3] = partition[0x5..0x5 + 3].try_into().unwrap();
            let start_sector = u32::from_le_bytes(partition[0x8..0x8 + 4].try_into().unwrap());
            let sector_count = u32::from_le_bytes(partition[0xC..0xC + 4].try_into().unwrap());

            match part_type {
                // Unused
                0 => None,

                // FAT16 (visible/hidden)
                0x06 | 0x16 => Some(Partition {
                    typ: PartitionType::Fat16,
                    start: start_sector,
                    size: sector_count,
                }),

                _ => {
                    eprintln!("Unsupported partition type {:x}", part_type);
                    None
                }
            }
            .map(|p| (ii as u8, p))
        })
        .collect::<Vec<_>>();

    if partitions.is_empty() {
        panic!("No partitions on disk");
    }
    let partition = if let Some(p) = partition {
        partitions.into_iter().find(|(i, _)| *i == p).unwrap()
    } else if partitions.len() > 1 {
        panic!("Multiple partitions, select with -p");
    } else {
        partitions.into_iter().next().unwrap()
    };

    let mut vbr = [0u8; 512];
    disk.seek(SeekFrom::Start(partition.1.start as u64 * 512))
        .unwrap();
    disk.read_exact(&mut vbr).unwrap();

    assert_eq!(
        &vbr[0x1FE..=0x1FF],
        &MBR_BOOT_SIG,
        "No VBR boot signature found"
    );

    let _oem_id = std::str::from_utf8(&vbr[0x03..0x03 + 8]).unwrap();
    let bytes_per_sector = u16::from_le_bytes(vbr[0x0B..0x0B + 2].try_into().unwrap());
    let sectors_per_cluster = vbr[0x0D];
    let reserved_sectors = u16::from_le_bytes(vbr[0x0E..0x0E + 2].try_into().unwrap());
    let fat_copies = vbr[0x10];
    let root_entry_count = u16::from_le_bytes(vbr[0x11..0x11 + 2].try_into().unwrap());
    let _small_sectors = u16::from_le_bytes(vbr[0x13..0x13 + 2].try_into().unwrap());
    let media_desc = vbr[0x15];
    let sectors_per_fat = u16::from_le_bytes(vbr[0x16..0x16 + 2].try_into().unwrap());
    let _sectors_per_track = u16::from_le_bytes(vbr[0x18..0x18 + 2].try_into().unwrap());
    let _number_of_heads = u16::from_le_bytes(vbr[0x1A..0x1A + 2].try_into().unwrap());
    let _hidden_sectors = u32::from_le_bytes(vbr[0x1C..0x1C + 4].try_into().unwrap());
    let _large_sectors = u32::from_le_bytes(vbr[0x20..0x20 + 4].try_into().unwrap());

    let fat_offset = (partition.1.start as u64 + reserved_sectors as u64) * bytes_per_sector as u64;
    let root_dir_offset =
        fat_offset + (fat_copies as u64 * sectors_per_fat as u64) * bytes_per_sector as u64;
    let data_offset = root_dir_offset + 32 * root_entry_count as u64;

    disk.seek(SeekFrom::Start(fat_offset)).unwrap();

    let mut fat_head = [0u8; 4];
    disk.read_exact(&mut fat_head).unwrap();
    let fat_head = [
        u16::from_le_bytes(fat_head[0..2].try_into().unwrap()),
        u16::from_le_bytes(fat_head[2..4].try_into().unwrap()),
    ];
    assert_eq!(fat_head[0], 0xFF00 | media_desc as u16);
    assert_eq!(fat_head[1] & 0x3FFF, 0x3FFF);

    disk.seek(SeekFrom::Start(root_dir_offset)).unwrap();

    let mut dir_entry = [0u8; 32];

    let mut lfn_name: Option<String> = None;

    if let Some(ex_file_path) = &ex_file_path {
        if ex_file_path.len() > 1 {
            todo!();
        }
    }

    for _ in 0..root_entry_count {
        disk.read_exact(&mut dir_entry).unwrap();
        match dir_entry[0] {
            0 => break,
            0xE5 => continue,
            0x05 => dir_entry[0] = 0xE5,
            0x20 => {
                eprintln!("Space at start of file! Skipping ...");
                continue;
            }
            _ => {}
        }

        let attributes = FatAttributes::from_bits(dir_entry[0x0B]).unwrap();
        if attributes == FatAttributes::LONG_NAME {
            let ordinal = dir_entry[0];
            if ordinal & 0x80 != 0 {
                todo!("Deleted LFN");
            }
            // TODO
            let _last = ordinal & 0x40 != 0;
            let _ordinal = ordinal & 0x3F;

            let _checksum = dir_entry[0x0D];

            let mut filename = [0u16; 13];
            for (fn_off, chr_off) in [
                0x01, 0x03, 0x05, 0x07, 0x09, 0x0E, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1C, 0x1E,
            ]
            .into_iter()
            .enumerate()
            {
                filename[fn_off] =
                    u16::from_le_bytes(dir_entry[chr_off..chr_off + 2].try_into().unwrap());
            }
            let filename = if let Some(end) = filename.iter().position(|c| *c == 0) {
                &filename[0..end]
            } else {
                &filename
            };
            let filename = String::from_utf16(filename).unwrap();
            lfn_name = match lfn_name {
                Some(pname) => Some(filename + &pname),
                None => Some(filename),
            };
        } else {
            let basename = std::str::from_utf8(&dir_entry[0x00..0x08])
                .unwrap()
                .trim_end_matches(' ');
            let extension = std::str::from_utf8(&dir_entry[0x08..0x0B])
                .unwrap()
                .trim_end_matches(' ');
            let creation_millis = dir_entry[0x0D];
            let creation_time = u16::from_le_bytes(dir_entry[0x0E..0x10].try_into().unwrap());
            let creation_date = u16::from_le_bytes(dir_entry[0x10..0x12].try_into().unwrap());
            let _creation =
                FatDateTime::from_date_time_fraction(creation_date, creation_time, creation_millis);

            let last_access_date = u16::from_le_bytes(dir_entry[0x12..0x14].try_into().unwrap());
            let _last_access = FatDateTime::from_date(last_access_date);

            let last_write_time = u16::from_le_bytes(dir_entry[0x16..0x18].try_into().unwrap());
            let last_write_date = u16::from_le_bytes(dir_entry[0x18..0x1A].try_into().unwrap());
            let _last_write = FatDateTime::from_date_time(last_write_date, last_write_time);

            let start_cluster = u16::from_le_bytes(dir_entry[0x1A..0x1C].try_into().unwrap());
            let file_size_bytes = u32::from_le_bytes(dir_entry[0x1C..0x20].try_into().unwrap());

            let filename = match lfn_name.take() {
                Some(lfn) => lfn,
                None if extension.is_empty() => basename.into(),
                None => format!("{}.{}", basename, extension),
            };

            /*
            let attribs = format!("{}{}{}{}{}{}",
                if attributes.contains(FatAttributes::READ_ONLY) { 'r' } else { '-' },
                if attributes.contains(FatAttributes::HIDDEN) { 'h' } else { '-' },
                if attributes.contains(FatAttributes::SYSTEM) { 's' } else { '-' },
                if attributes.contains(FatAttributes::VOLUME_ID) { 'v' } else { '-' },
                if attributes.contains(FatAttributes::DIRECTORY) { 'd' } else { '-' },
                if attributes.contains(FatAttributes::ARCHIVE) { 'a' } else { '-' });


            println!("{:24} {} {}  {}  {}  {} {}", filename, attribs, creation, last_access, last_write, starting_cluster, file_size_bytes);*/

            if ex_file_path.as_ref().map(|e| e[0]) == Some(&filename) {
                println!("Found file {}, extracting ...", filename);
                assert!(!attributes.contains(FatAttributes::DIRECTORY));

                let mut output = File::create(filename).unwrap();

                if file_size_bytes > 0 {
                    let cluster_size = sectors_per_cluster as u64 * bytes_per_sector as u64;
                    let mut copy_buffer = vec![0u8; cluster_size.try_into().unwrap()];

                    let mut remaining_bytes = file_size_bytes;
                    let mut fat_index = start_cluster;

                    loop {
                        disk.seek(SeekFrom::Start(fat_offset + fat_index as u64 * 2))
                            .unwrap();
                        let mut fat_entry = [0u8; 2];
                        disk.read_exact(&mut fat_entry).unwrap();
                        let fat_entry = u16::from_le_bytes(fat_entry);

                        let last = remaining_bytes as u64 <= cluster_size;

                        disk.seek(SeekFrom::Start(
                            data_offset + (fat_index as u64 - 2) * cluster_size as u64,
                        ))
                        .unwrap();

                        let read_len = if last {
                            remaining_bytes as u64
                        } else {
                            cluster_size
                        };
                        disk.read_exact(&mut copy_buffer[0..read_len.try_into().unwrap()])
                            .unwrap();
                        remaining_bytes -= read_len as u32;

                        output
                            .write_all(&copy_buffer[0..read_len as usize])
                            .unwrap();

                        match fat_entry {
                            0x0000 => panic!("Got free cluster"),
                            0x0001 | 0x0002 => panic!("Invalid FAT entry"),
                            0x0003..=0xFFEF => {
                                if last {
                                    panic!();
                                }
                                fat_index = fat_entry;
                            }
                            0xFFF7 => panic!("Bad sectors"),
                            0xFFF8..=0xFFFF => {
                                if !last {
                                    panic!();
                                }
                                break;
                            }
                            _ => panic!("Unknown FAT entry"),
                        }
                    }
                }

                break;
            }
        }
    }
}
