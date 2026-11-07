//! Screenshots go to the model scaled down to a size it handles well, and
//! the coordinates it answers with are in that scaled space: [Mapping]
//! turns them back into the screen's real pixels (HiDPI included: the real
//! pixels are the output's physical ones).

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Limits {
    pub max_long_side: u32,
    pub max_pixels: u64,
}

impl Default for Limits {
    fn default() -> Self {
        Limits { max_long_side: 1280, max_pixels: 1_150_000 }
    }
}

impl Limits {
    /// The largest size within the limits with `w`×`h`'s aspect, never
    /// larger than it.
    pub fn fit(&self, w: u32, h: u32) -> (u32, u32) {
        if w == 0 || h == 0 {
            return (w, h);
        }
        let long = w.max(h) as f64;
        let by_side = self.max_long_side as f64 / long;
        let by_area = (self.max_pixels as f64 / (w as f64 * h as f64)).sqrt();
        let s = by_side.min(by_area).min(1.0);
        if s >= 1.0 {
            return (w, h);
        }
        let sw = ((w as f64 * s).floor() as u32).max(1);
        let sh = ((h as f64 * s).floor() as u32).max(1);
        (sw, sh)
    }
}

/// Between the scaled space the model sees and the screen's real pixels.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Mapping {
    pub real_w: u32,
    pub real_h: u32,
    pub scaled_w: u32,
    pub scaled_h: u32,
}

impl Mapping {
    pub fn new(limits: &Limits, real_w: u32, real_h: u32) -> Mapping {
        let (scaled_w, scaled_h) = limits.fit(real_w, real_h);
        Mapping { real_w, real_h, scaled_w, scaled_h }
    }

    /// A point the model gave, checked to be on the screenshot.
    pub fn to_real(&self, x: i64, y: i64) -> Result<(i32, i32), String> {
        if x < 0 || y < 0 || x >= self.scaled_w as i64 || y >= self.scaled_h as i64 {
            return Err(format!(
                "({x}, {y}) is off the screen: x must be within 0..{} and y within 0..{} (the screenshot's size)",
                self.scaled_w - 1,
                self.scaled_h - 1
            ));
        }
        Ok((Self::map(x, self.scaled_w, self.real_w), Self::map(y, self.scaled_h, self.real_h)))
    }

    /// A real point in the scaled space.
    pub fn to_scaled(&self, x: i32, y: i32) -> (i32, i32) {
        (Self::map(x as i64, self.real_w, self.scaled_w), Self::map(y as i64, self.real_h, self.scaled_h))
    }

    fn map(v: i64, from: u32, to: u32) -> i32 {
        let r = v * to as i64 / from as i64;
        r.clamp(0, to as i64 - 1) as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fits() {
        let l = Limits::default();
        assert_eq!(l.fit(2560, 1600), (1280, 800));
        assert_eq!(l.fit(2560, 1440), (1280, 720));
        assert_eq!(l.fit(1920, 1080), (1280, 720));
        // within both limits: as it is
        assert_eq!(l.fit(1024, 768), (1024, 768));
        // portrait: the long side is the height
        assert_eq!(l.fit(1080, 1920), (720, 1280));
        // the pixel limit binds before the side limit for squares
        let (w, h) = l.fit(2000, 2000);
        assert_eq!((w, h), (1072, 1072));
        assert!(w as u64 * h as u64 <= l.max_pixels);
        let small = Limits { max_long_side: 320, max_pixels: 1_000_000 };
        assert_eq!(small.fit(640, 400), (320, 200));
    }

    #[test]
    fn maps() {
        let m = Mapping::new(&Limits::default(), 2560, 1600);
        assert_eq!(m.to_real(640, 312), Ok((1280, 624)));
        assert_eq!(m.to_real(0, 0), Ok((0, 0)));
        assert_eq!(m.to_real(1279, 799), Ok((2558, 1598)));
        assert_eq!(m.to_scaled(1281, 625), (640, 312));
        assert_eq!(m.to_scaled(1280, 624), (640, 312));
        assert!(m.to_real(1280, 0).unwrap_err().contains("x must be within 0..1279 and y within 0..799"));
        assert!(m.to_real(-1, 0).is_err());
        // unscaled: identity
        let id = Mapping::new(&Limits::default(), 1024, 768);
        assert_eq!(id.to_real(500, 300), Ok((500, 300)));
        assert_eq!(id.to_scaled(500, 300), (500, 300));
        // every scaled point round-trips
        for x in 0..1280 {
            let (rx, _) = m.to_real(x, 0).unwrap();
            assert_eq!(m.to_scaled(rx, 0).0, x as i32);
        }
    }
}
