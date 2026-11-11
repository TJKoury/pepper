//! The ANISE side of the PEPPER/ANISE speed comparison (bench/compare_anise.py).
//!
//! The same queries as test/core/bench_compare.f90 (PEPPER): the same
//! kernels, the same 100000 pseudo-random epochs over 20 years from J2000
//! (Park-Miller, seed 4242), the same sums of the results as checksums.
//! Usage: bench_anise KERNEL_DIR. One line per operation:
//! key|operations|microseconds per operation|checksum

use anise::constants::celestial_objects::{EARTH, MOON};
use anise::constants::frames::{
    EARTH_ITRF93, EARTH_J2000, IAU_MARS_FRAME, MARS_BARYCENTER_J2000, MARS_J2000, MOON_J2000,
    MOON_ME_DE440_ME421_FRAME, SUN_J2000,
};
use anise::constants::orientations::{J2000, J2000_TO_ECLIPJ2000_ANGLE_RAD};
use anise::naif::kpl::parser::{convert_fk, convert_tpc};
use anise::prelude::*;
use std::hint::black_box;
use std::time::Instant;

const NQ: usize = 100_000;
const YEAR: f64 = 365.25 * 86400.0;
const STEP: f64 = 60.0; // spacing of the sequential epochs (s)

/// The epochs of bench_compare.f90: span*(random(1000000)-1)/1e6.
fn random_epochs(n: usize, span: f64) -> Vec<f64> {
    let mut seed: i64 = 4242;
    (0..n)
        .map(|_| {
            seed = (seed * 48271) % 2147483647;
            let r = seed % 1_000_000 + 1;
            span * ((r - 1) as f64) / 1.0e6
        })
        .collect()
}

fn report(key: &str, n: usize, seconds: f64, checksum: f64) {
    println!("{}|{}|{:.5e}|{:.15e}", key, n, 1.0e6 * seconds / n as f64, checksum);
}

fn main() {
    let kdir = std::env::args().nth(1).expect("usage: bench_anise KERNEL_DIR");
    let k = |f: &str| format!("{kdir}/{f}");

    // The text kernels must be converted to ANISE's own formats first (PCA
    // for the text PCK, EPA for the frame kernel); the conversion is timed
    // on its own.
    let t0 = Instant::now();
    // ANISE keeps only the bodies with a GM, and pck00011.tpc has none: the
    // GMs of Mars and of its barycenter (whose BODY4_NUT_PREC_ANGLES the
    // IAU_MARS rotation uses) and of the Moon (MOON_ME needs its entry) are
    // given here, DE440's values. The GMs do
    // not enter the rotations.
    let gm = std::env::temp_dir().join("bench_anise_gm.tpc");
    std::fs::write(
        &gm,
        "KPL/PCK\n\\begindata\nBODY499_GM = ( 4.282837362069909E+04 )\n\
         BODY4_GM = ( 4.282837581575610E+04 )\nBODY301_GM = ( 4.902800118457549E+03 )\nBODY3_GM = ( 4.035032355022598E+05 )\n\\begintext\n",
    )
    .expect("write the GM kernel");
    let gm = gm.to_str().unwrap().to_owned();
    let pca = convert_tpc(k("pck00011.tpc"), gm).expect("convert_tpc");
    let epa = convert_fk(k("moon_de440_250416.tf"), false).expect("convert_fk");
    report("anise_convert_text", 1, t0.elapsed().as_secs_f64(), 0.0);

    // Loading: the binary kernels (read whole into memory) and the converted
    // data sets; 10 times, as bench_compare.f90 does.
    let nsets = 10;
    let mut t = 0.0;
    let mut almanac = Almanac::default();
    let _ = &almanac;
    for _ in 0..nsets {
        let t0 = Instant::now();
        almanac = Almanac::default()
            .load(&k("de421.bsp"))
            .and_then(|a| a.load(&k("earth_latest_high_prec.bpc")))
            .and_then(|a| a.load(&k("moon_pa_de440_200625.bpc")))
            .expect("load")
            .with_planetary_data(pca.clone())
            .with_euler_parameters(epa.clone());
        t += t0.elapsed().as_secs_f64();
    }
    report("load_set", nsets, t, 0.0);

    let ets = random_epochs(NQ, 20.0 * YEAR);
    let random: Vec<Epoch> = ets.iter().map(|&et| Epoch::from_et_seconds(et)).collect();
    let seq: Vec<Epoch> = (0..NQ).map(|i| Epoch::from_et_seconds(i as f64 * STEP)).collect();
    states(&almanac, &random, "");
    states(&almanac, &seq, "_seq");
    frames(&almanac, &random, "");
    frames(&almanac, &seq, "_seq");
}

/// J2000 to ECLIPJ2000: SPICE's fixed rotation about X by the obliquity of
/// J2000 (ANISE 0.10.6 knows ECLIPJ2000 only from its own PCA file).
fn to_eclip(v: [f64; 3]) -> [f64; 3] {
    let (s, c) = J2000_TO_ECLIPJ2000_ANGLE_RAD.sin_cos();
    [v[0], c * v[1] + s * v[2], -s * v[1] + c * v[2]]
}

fn states(almanac: &Almanac, epochs: &[Epoch], suffix: &str) {
    // SPKEZR Moon from Earth, J2000, no correction (ANISE takes IDs, not names)
    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let s = almanac.translate(MOON_J2000, EARTH_J2000, e, Aberration::NONE).unwrap();
        sum += s.radius_km.x;
    }
    report(&format!("spkezr_moon{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);

    // SPKGEO Moon from Earth: ANISE's spk_ezr alias
    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let s = almanac.spk_ezr(MOON, e, J2000, EARTH, Aberration::NONE).unwrap();
        sum += s.radius_km.x;
    }
    report(&format!("spkgeo_moon{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);

    // SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S
    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let s = almanac
            .translate(MARS_BARYCENTER_J2000, EARTH_J2000, e, Aberration::LT_S)
            .unwrap();
        let r = to_eclip([s.radius_km.x, s.radius_km.y, s.radius_km.z]);
        let v = to_eclip([s.velocity_km_s.x, s.velocity_km_s.y, s.velocity_km_s.z]);
        sum += black_box(r)[0] + 0.0 * black_box(v)[0];
    }
    report(&format!("spkezr_mars_lts{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);

    // SPKPOS Sun from Earth, J2000, CN+S
    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let s = almanac.translate(SUN_J2000, EARTH_J2000, e, Aberration::CN_S).unwrap();
        sum += s.radius_km.x;
    }
    report(&format!("spkpos_sun_cns{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);
}

// MOON_ME is, in moon_de440_250416.tf, the identity alias of
// MOON_ME_DE440_ME421 (31009). ANISE 0.10.6's convert_fk maps the alias to
// the Moon's body ID (31001 -> 301, an identity) instead, so MOON_ME would be
// wrong; ANISE's side asks for MOON_ME_DE440_ME421 itself.
fn frames(almanac: &Almanac, epochs: &[Epoch], suffix: &str) {
    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let d = almanac.rotate(EARTH_J2000, EARTH_ITRF93, e).unwrap();
        sum += d.rot_mat[(0, 0)];
    }
    report(&format!("pxform_itrf93{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);

    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let d = almanac.rotate(EARTH_J2000, EARTH_ITRF93, e).unwrap();
        sum += d.rot_mat_dt.unwrap()[(0, 0)];
    }
    report(&format!("sxform_itrf93{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);

    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let d = almanac.rotate(MARS_J2000, IAU_MARS_FRAME, e).unwrap();
        sum += d.rot_mat[(0, 0)];
    }
    report(&format!("pxform_iau_mars{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);

    let mut sum = 0.0;
    let t0 = Instant::now();
    for &e in epochs {
        let d = almanac.rotate(MOON_ME_DE440_ME421_FRAME, MOON_J2000, e).unwrap();
        sum += d.rot_mat[(0, 0)];
    }
    report(&format!("pxform_moon_me{suffix}"), NQ, t0.elapsed().as_secs_f64(), sum);
}
