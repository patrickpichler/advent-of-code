struct Section {
    from: u8,
    to: u8,
}

impl Section {
    fn contains(&self, other: &Section) -> bool {
        return self.from <= other.from
            && self.to >= other.from
            && self.from <= other.to
            && self.to >= other.to;
    }

    fn overlaps(&self, other: &Section) -> bool {
        return !(self.to < other.from
            || other.to < self.from
            || self.from > other.to
            || other.from > self.to);
    }
}

impl TryFrom<&str> for Section {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut split = value.splitn(2, '-');
        let first = split.next().ok_or("no first value")?;
        let second = split.next().ok_or("no second value")?;

        return Ok(Section {
            from: str::parse::<u8>(first).map_err(|err| err.to_string())?,
            to: str::parse::<u8>(second).map_err(|err| err.to_string())?,
        });
    }
}

struct Line {
    first: Section,
    second: Section,
}

impl Line {
    fn has_contained_section(&self) -> bool {
        return self.first.contains(&self.second) || self.second.contains(&self.first);
    }

    fn has_overlapping_section(&self) -> bool {
        return self.first.overlaps(&self.second)
    }
}

impl TryFrom<&str> for Line {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut split = value.splitn(2, ',');

        return Ok(Line {
            first: split
                .next()
                .ok_or("no first value".to_string())
                .and_then(|s| s.try_into())?,
            second: split
                .next()
                .ok_or("no second value".to_string())
                .and_then(|s| s.try_into())?,
        });
    }
}

fn main() {
    let lines = include_str!("../input.txt")
        .lines()
        .map(|l| Line::try_from(l).unwrap());

    let part1 = lines.clone().filter(|l| l.has_contained_section()).count();

    println!("{}", part1);

    let part2 = lines.clone().filter(|l| l.has_overlapping_section()).count();

    println!("{}", part2);
}
