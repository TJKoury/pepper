#!/usr/bin/env python3
"""
Generate an SVG logo for PEPPER:
  Planetary Ephemerides, Positions, Pointing, Events & Rotations.
  A modern Fortran reimagining of SPICELIB.

The logo is a tilted pepper shaker spraying planets instead of pepper,
with the PEPPER name underneath.

Usage:
    python make_logo.py                 # writes pepper_logo.svg
    python make_logo.py -o out.svg      # custom output path
    python make_logo.py --no-tagline    # name only
    python make_logo.py --transparent   # no space background
    python make_logo.py --down          # shaker pointing down (pepper_logo_down.svg)
"""

import argparse
import math
import random

W, H = 1000, 1000

# ---------------------------------------------------------------------------
# Palette
# ---------------------------------------------------------------------------
SPACE_INNER = "#1d2a5c"
SPACE_OUTER = "#070b1f"
TEXT_LIGHT = "#f6efe2"
TEXT_DARK = "#1a2040"
ACCENT = "#e8553d"  # pepper red, used for the acronym letters

_uid = 0


def uid(prefix):
    """Unique id for per-instance clip paths."""
    global _uid
    _uid += 1
    return f"{prefix}{_uid}"


def rotate(x, y, deg):
    """Rotate a point about the origin (SVG convention: +deg is clockwise)."""
    t = math.radians(deg)
    return x * math.cos(t) - y * math.sin(t), x * math.sin(t) + y * math.cos(t)


# ---------------------------------------------------------------------------
# Shared <defs>
# ---------------------------------------------------------------------------
def defs():
    return """
  <defs>
    <radialGradient id="space" cx="50%" cy="42%" r="70%">
      <stop offset="0" stop-color="{inner}"/>
      <stop offset="1" stop-color="{outer}"/>
    </radialGradient>

    <!-- generic 3D shading laid over every planet -->
    <radialGradient id="shade" cx="0.5" cy="0.5" r="0.5" fx="0.32" fy="0.28">
      <stop offset="0" stop-color="#fff" stop-opacity="0.45"/>
      <stop offset="0.35" stop-color="#fff" stop-opacity="0.05"/>
      <stop offset="0.7" stop-color="#000" stop-opacity="0.15"/>
      <stop offset="1" stop-color="#000" stop-opacity="0.6"/>
    </radialGradient>

    <radialGradient id="glow" cx="0.5" cy="0.5" r="0.5">
      <stop offset="0" stop-color="#ffd9a0" stop-opacity="0.55"/>
      <stop offset="1" stop-color="#ffd9a0" stop-opacity="0"/>
    </radialGradient>

    <!-- ceramic body of the shaker -->
    <linearGradient id="ceramic" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0" stop-color="#cfc6b8"/>
      <stop offset="0.22" stop-color="#fffaf0"/>
      <stop offset="0.55" stop-color="#efe6d6"/>
      <stop offset="1" stop-color="#a99f92"/>
    </linearGradient>

    <!-- chrome cap -->
    <linearGradient id="chrome" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0" stop-color="#6d7383"/>
      <stop offset="0.25" stop-color="#f4f6fa"/>
      <stop offset="0.45" stop-color="#b9bfcc"/>
      <stop offset="0.7" stop-color="#e3e7ee"/>
      <stop offset="1" stop-color="#5c6272"/>
    </linearGradient>

    <linearGradient id="collar" x1="0" y1="0" x2="1" y2="0">
      <stop offset="0" stop-color="#4b505e"/>
      <stop offset="0.3" stop-color="#d9dde5"/>
      <stop offset="0.6" stop-color="#9097a6"/>
      <stop offset="1" stop-color="#3f4350"/>
    </linearGradient>

    <linearGradient id="title" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#fff7e6"/>
      <stop offset="1" stop-color="#f0c987"/>
    </linearGradient>

    <filter id="softglow" x="-50%" y="-50%" width="200%" height="200%">
      <feGaussianBlur stdDeviation="3"/>
    </filter>
  </defs>""".format(inner=SPACE_INNER, outer=SPACE_OUTER)


# ---------------------------------------------------------------------------
# Background
# ---------------------------------------------------------------------------
def background(seed=7):
    rng = random.Random(seed)
    out = [f'<rect x="0" y="0" width="{W}" height="{H}" rx="120" fill="url(#space)"/>']
    for _ in range(140):
        x, y = rng.uniform(40, W - 40), rng.uniform(40, H - 40)
        r = rng.choice([0.8, 1.0, 1.2, 1.6, 2.0])
        op = rng.uniform(0.35, 0.95)
        out.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="#fff" opacity="{op:.2f}"/>')
    # a few four-point sparkles
    for x, y, s in [(140, 160, 14), (860, 120, 10), (120, 560, 9), (900, 640, 12), (470, 110, 8)]:
        out.append(sparkle(x, y, s, "#fff3d6", 0.9))
    return "\n  ".join(out)


def sparkle(x, y, s, color, op):
    k = s * 0.18
    d = (f"M {x} {y - s} Q {x + k} {y - k} {x + s} {y} Q {x + k} {y + k} {x} {y + s} "
         f"Q {x - k} {y + k} {x - s} {y} Q {x - k} {y - k} {x} {y - s} Z")
    return f'<path d="{d}" fill="{color}" opacity="{op}"/>'


# ---------------------------------------------------------------------------
# Planets
# ---------------------------------------------------------------------------
def clip_circle(cx, cy, r):
    cid = uid("c")
    return cid, f'<clipPath id="{cid}"><circle cx="{cx}" cy="{cy}" r="{r}"/></clipPath>'


def shade(cx, cy, r):
    return f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="url(#shade)"/>'


def planet_plain(cx, cy, r, color):
    return f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="{color}"/>' + shade(cx, cy, r)


def planet_moon(cx, cy, r):
    cid, clip = clip_circle(cx, cy, r)
    craters = "".join(
        f'<circle cx="{cx + dx * r}" cy="{cy + dy * r}" r="{s * r}" fill="#8e8e98" opacity="0.8"/>'
        for dx, dy, s in [(-0.35, -0.2, 0.22), (0.3, 0.25, 0.28), (0.15, -0.45, 0.12), (-0.25, 0.5, 0.15)]
    )
    return (f'{clip}<circle cx="{cx}" cy="{cy}" r="{r}" fill="#c9c9d1"/>'
            f'<g clip-path="url(#{cid})">{craters}</g>' + shade(cx, cy, r))


def planet_mars(cx, cy, r):
    cid, clip = clip_circle(cx, cy, r)
    return (f'{clip}<circle cx="{cx}" cy="{cy}" r="{r}" fill="#d9603b"/>'
            f'<g clip-path="url(#{cid})">'
            f'<ellipse cx="{cx - 0.2 * r}" cy="{cy + 0.15 * r}" rx="{0.6 * r}" ry="{0.25 * r}" fill="#a8402a" opacity="0.7"/>'
            f'<ellipse cx="{cx}" cy="{cy - 0.92 * r}" rx="{0.45 * r}" ry="{0.18 * r}" fill="#fff" opacity="0.85"/>'
            f'</g>' + shade(cx, cy, r))


def planet_earth(cx, cy, r):
    cid, clip = clip_circle(cx, cy, r)

    def blob(pts):
        p = [(cx + x * r, cy + y * r) for x, y in pts]
        d = f"M {p[0][0]:.1f} {p[0][1]:.1f} "
        for i in range(1, len(p) - 1, 2):
            d += f"Q {p[i][0]:.1f} {p[i][1]:.1f} {p[i + 1][0]:.1f} {p[i + 1][1]:.1f} "
        return f'<path d="{d}Z" fill="#4caf63"/>'

    land = (blob([(-0.7, -0.5), (-0.3, -0.9), (0.1, -0.5), (0.0, -0.1), (-0.3, 0.2), (-0.7, 0.1), (-0.7, -0.5)])
            + blob([(0.25, 0.0), (0.7, -0.2), (0.8, 0.3), (0.5, 0.8), (0.2, 0.5), (0.1, 0.2), (0.25, 0.0)]))
    clouds = (f'<ellipse cx="{cx - 0.1 * r}" cy="{cy + 0.45 * r}" rx="{0.5 * r}" ry="{0.08 * r}" fill="#fff" opacity="0.7"/>'
              f'<ellipse cx="{cx + 0.3 * r}" cy="{cy - 0.6 * r}" rx="{0.35 * r}" ry="{0.07 * r}" fill="#fff" opacity="0.7"/>')
    return (f'{clip}<circle cx="{cx}" cy="{cy}" r="{r}" fill="#2f7fd6"/>'
            f'<g clip-path="url(#{cid})">{land}{clouds}</g>'
            f'<circle cx="{cx}" cy="{cy}" r="{r + 2}" fill="none" stroke="#9fd3ff" stroke-opacity="0.35" stroke-width="3"/>'
            + shade(cx, cy, r))


def planet_jupiter(cx, cy, r, tilt=-15):
    cid, clip = clip_circle(cx, cy, r)
    bands = [(-0.75, 0.14, "#b9825a"), (-0.45, 0.18, "#f3dcb8"), (-0.2, 0.16, "#c98a5c"),
             (0.05, 0.2, "#f7e6c8"), (0.3, 0.14, "#a86a45"), (0.55, 0.18, "#e9c9a0"),
             (0.8, 0.14, "#b9825a")]
    rects = "".join(
        f'<rect x="{cx - 1.2 * r}" y="{cy + y * r}" width="{2.4 * r}" height="{h * r}" fill="{c}"/>'
        for y, h, c in bands
    )
    spot = f'<ellipse cx="{cx + 0.3 * r}" cy="{cy + 0.38 * r}" rx="{0.22 * r}" ry="{0.11 * r}" fill="#c4452d"/>'
    return (f'{clip}<circle cx="{cx}" cy="{cy}" r="{r}" fill="#e2b98c"/>'
            f'<g clip-path="url(#{cid})"><g transform="rotate({tilt} {cx} {cy})">{rects}{spot}</g></g>'
            + shade(cx, cy, r))


def planet_saturn(cx, cy, r, tilt=-22):
    rx, ry = 2.15 * r, 0.6 * r

    def ring(half):
        # half="back" -> upper arc, drawn behind the planet; "front" -> lower arc
        if half == "back":
            d = f"M {cx - rx} {cy} A {rx} {ry} 0 0 1 {cx + rx} {cy}"
        else:
            d = f"M {cx + rx} {cy} A {rx} {ry} 0 0 1 {cx - rx} {cy}"
        d2 = d.replace(f"{rx} {ry}", f"{rx * 0.82} {ry * 0.82}")
        d2 = (d2.replace(f"{cx - rx} ", f"{cx - rx * 0.82} ").replace(f"{cx + rx} ", f"{cx + rx * 0.82} "))
        return (f'<path d="{d}" fill="none" stroke="#d8c08f" stroke-width="{0.2 * r:.1f}" stroke-linecap="round"/>'
                f'<path d="{d2}" fill="none" stroke="#f3e2b8" stroke-width="{0.12 * r:.1f}" stroke-linecap="round" opacity="0.9"/>')

    cid, clip = clip_circle(cx, cy, r)
    bands = "".join(
        f'<rect x="{cx - 1.2 * r}" y="{cy + y * r}" width="{2.4 * r}" height="{0.16 * r}" fill="#c9a66b" opacity="0.6"/>'
        for y in (-0.55, -0.1, 0.4)
    )
    body = (f'{clip}<circle cx="{cx}" cy="{cy}" r="{r}" fill="#ecd29c"/>'
            f'<g clip-path="url(#{cid})"><g transform="rotate({tilt} {cx} {cy})">{bands}</g></g>'
            + shade(cx, cy, r))
    return (f'<g transform="rotate({tilt} {cx} {cy})">{ring("back")}</g>'
            f'{body}'
            f'<g transform="rotate({tilt} {cx} {cy})">{ring("front")}</g>')


def planet_neptune(cx, cy, r):
    cid, clip = clip_circle(cx, cy, r)
    return (f'{clip}<circle cx="{cx}" cy="{cy}" r="{r}" fill="#3f6fe0"/>'
            f'<g clip-path="url(#{cid})">'
            f'<rect x="{cx - r}" y="{cy - 0.15 * r}" width="{2 * r}" height="{0.14 * r}" fill="#7ea4ff" opacity="0.6"/>'
            f'<ellipse cx="{cx - 0.25 * r}" cy="{cy + 0.35 * r}" rx="{0.2 * r}" ry="{0.1 * r}" fill="#22408f"/>'
            f'</g>' + shade(cx, cy, r))


PLANETS = {
    "moon": planet_moon,
    "mars": planet_mars,
    "earth": planet_earth,
    "jupiter": planet_jupiter,
    "saturn": planet_saturn,
    "neptune": planet_neptune,
    "venus": lambda x, y, r: planet_plain(x, y, r, "#e9c46a"),
    "mercury": lambda x, y, r: planet_plain(x, y, r, "#a39a92"),
    "uranus": lambda x, y, r: planet_plain(x, y, r, "#8fe0e0"),
}


# ---------------------------------------------------------------------------
# Pepper shaker (drawn upright in local coords, origin = bottom centre)
# ---------------------------------------------------------------------------
CAP_TOP = -350  # local y of the top of the dome


def shaker():
    body = ("M -72 -240 C -72 -200 -92 -160 -92 -90 L -92 -24 Q -92 0 -68 0 "
            "L 68 0 Q 92 0 92 -24 L 92 -90 C 92 -160 72 -200 72 -240 Z")
    highlight = ("M -62 -225 C -64 -190 -78 -150 -78 -95 L -78 -40 Q -78 -24 -70 -24 "
                 "L -66 -24 Q -62 -24 -62 -40 L -62 -95 C -62 -150 -52 -190 -50 -225 Z")
    dome = f"M -80 -264 C -80 -322 -42 {CAP_TOP} 0 {CAP_TOP} C 42 {CAP_TOP} 80 -322 80 -264 Z"
    holes = [(0, -335), (-22, -326), (22, -326), (-42, -308), (-14, -312), (14, -312), (42, -308),
             (-58, -286), (-29, -290), (0, -292), (29, -290), (58, -286)]
    hole_svg = "".join(
        f'<ellipse cx="{x}" cy="{y}" rx="4.6" ry="3.2" fill="#1b1e28"/>'
        f'<ellipse cx="{x}" cy="{y + 1.4}" rx="3.4" ry="1.2" fill="#fff" opacity="0.35"/>'
        for x, y in holes
    )
    # Greek evil eye (mati / nazar) on the body: concentric navy, white,
    # light-blue and black discs with a glassy highlight
    emblem = ('<g transform="translate(0 -120)">'
              '<circle cx="0" cy="0" r="40" fill="#1846a8" stroke="#0e2a6b" stroke-width="2.5"/>'
              '<circle cx="0" cy="0" r="29" fill="#ffffff"/>'
              '<circle cx="0" cy="0" r="19" fill="#5fb4ee"/>'
              '<circle cx="0" cy="0" r="10" fill="#0b0f1e"/>'
              '<ellipse cx="-14" cy="-20" rx="11" ry="6" fill="#fff" opacity="0.55" transform="rotate(-35 -14 -20)"/>'
              '<circle cx="4" cy="-4" r="3" fill="#fff" opacity="0.9"/>'
              '</g>')
    return (
        f'<path d="{body}" fill="url(#ceramic)" stroke="#5b5348" stroke-width="3"/>'
        f'<path d="{highlight}" fill="#fff" opacity="0.55"/>'
        f'{emblem}'
        f'<rect x="-84" y="-266" width="168" height="28" rx="7" fill="url(#collar)" stroke="#2f323c" stroke-width="2.5"/>'
        f'<rect x="-84" y="-258" width="168" height="3" fill="#fff" opacity="0.35"/>'
        f'<path d="{dome}" fill="url(#chrome)" stroke="#3a3e4a" stroke-width="2.5"/>'
        f'<path d="M -55 -280 C -52 -315 -30 -336 -8 -340" fill="none" stroke="#fff" stroke-width="5" '
        f'stroke-linecap="round" opacity="0.6"/>'
        f'{hole_svg}'
    )


# ---------------------------------------------------------------------------
# Composition
# ---------------------------------------------------------------------------
# Shaker placement: origin = bottom centre of the body, tilt in degrees clockwise
# from upright. "up" sprays planets up and to the right; "down" has the shaker
# in the upper left, cap pointing down, pouring planets down toward the name.
POSES = {
    "up": {"origin": (285, 655), "tilt": 50},
    "down": {"origin": (200, 130), "tilt": 130},
}

# (distance from cap tip, angle offset from shaker axis [deg], kind, radius)
SPRAY = [
    (62, -12, "dust", 4), (58, 14, "dust", 3.5), (95, 4, "mercury", 9),
    (112, -22, "dust", 3), (122, 22, "dust", 4),
    (150, -8, "mars", 16), (160, 20, "moon", 13),
    (200, -20, "venus", 15), (225, 2, "earth", 27), (190, 36, "dust", 3),
    (285, -24, "neptune", 21), (250, 30, "uranus", 18),
    (320, 18, "saturn", 34), (400, -6, "jupiter", 48),
    (350, -34, "dust", 3), (300, 40, "dust", 3), (420, 14, "dust", 4),
]


def spray(origin, tilt):
    ox, oy = origin
    tx, ty = rotate(0, CAP_TOP, tilt)
    tip = (ox + tx, oy + ty)
    axis = math.degrees(math.atan2(ty, tx))  # direction the cap points

    trails, bodies = [], []
    # glow puff right at the holes
    gx, gy = tip[0] + 30 * math.cos(math.radians(axis)), tip[1] + 30 * math.sin(math.radians(axis))
    trails.append(f'<circle cx="{gx:.1f}" cy="{gy:.1f}" r="95" fill="url(#glow)"/>')

    for dist, off, kind, r in SPRAY:
        a = math.radians(axis + off)
        cx, cy = tip[0] + dist * math.cos(a), tip[1] + dist * math.sin(a)
        cx, cy = round(cx, 1), round(cy, 1)
        # motion trail pointing back toward the shaker
        tl = min(dist * 0.55, 30 + r * 2.2)
        bx, by = cx - tl * math.cos(a), cy - tl * math.sin(a)
        if kind == "dust":
            trails.append(f'<line x1="{bx:.1f}" y1="{by:.1f}" x2="{cx}" y2="{cy}" stroke="#ffe7b8" '
                          f'stroke-width="{r * 0.9:.1f}" stroke-linecap="round" opacity="0.25"/>')
            bodies.append(f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="#ffe7b8"/>')
        else:
            trails.append(f'<line x1="{bx:.1f}" y1="{by:.1f}" x2="{cx}" y2="{cy}" stroke="#ffffff" '
                          f'stroke-width="{r * 1.3:.1f}" stroke-linecap="round" opacity="0.08" filter="url(#softglow)"/>')
            bodies.append(PLANETS[kind](cx, cy, r))

    return "\n  ".join(trails), "\n  ".join(bodies)


def wordmark(tagline=True, dark=False):
    fill = "url(#title)" if not dark else TEXT_DARK
    sub = TEXT_LIGHT if not dark else TEXT_DARK
    font = "'Futura', 'Avenir Next', 'Montserrat', 'Helvetica Neue', Arial, sans-serif"
    out = [f'<text x="{W / 2 + 9}" y="868" text-anchor="middle" font-family="{font}" font-weight="800" '
           f'font-size="138" letter-spacing="18" fill="{fill}">PEPPER</text>']
    if tagline:
        words = ["Planetary", "Ephemerides", "Positions", "Pointing", "Events &amp;", "Rotations"]
        spans = []
        for i, w in enumerate(words):
            sep = " · " if 0 < i < len(words) - 1 else (" " if i else "")
            spans.append(f'{sep}<tspan fill="{ACCENT}" font-weight="700">{w[0]}</tspan>{w[1:]}')
        out.append(f'<text x="{W / 2}" y="926" text-anchor="middle" font-family="{font}" font-size="23" '
                   f'letter-spacing="0.5" fill="{sub}">{"".join(spans)}</text>')
    return "\n  ".join(out)


def build_svg(tagline=True, transparent=False, pose="up"):
    origin, tilt = POSES[pose]["origin"], POSES[pose]["tilt"]
    trails, bodies = spray(origin, tilt)
    ox, oy = origin
    parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">',
        "  <title>PEPPER</title>",
        defs(),
        "  " + (background() if not transparent else ""),
        "  " + trails,
        f'  <g transform="translate({ox} {oy}) rotate({tilt})">{shaker()}</g>',
        "  " + bodies,
        "  " + wordmark(tagline, dark=transparent),
        "</svg>",
    ]
    return "\n".join(parts) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-o", "--output", default=None,
                    help="output SVG path (default: pepper_logo.svg, or pepper_logo_down.svg with --down)")
    ap.add_argument("--down", action="store_true", help="shaker points down, pouring planets downward")
    ap.add_argument("--no-tagline", action="store_true", help="omit the acronym/tagline text")
    ap.add_argument("--transparent", action="store_true", help="no background; dark text")
    args = ap.parse_args()

    if args.output is None:
        args.output = "pepper_logo_down.svg" if args.down else "pepper_logo.svg"
    svg = build_svg(tagline=not args.no_tagline, transparent=args.transparent,
                    pose="down" if args.down else "up")
    with open(args.output, "w", encoding="utf-8") as f:
        f.write(svg)
    print(f"wrote {args.output}")


if __name__ == "__main__":
    main()
