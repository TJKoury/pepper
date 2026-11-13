// The hero's logo in 3D: the model of media/make_logo_3d.py (pepper_logo.glb)
// rendered with three.js in the logo's circle, the scene swaying from side
// to side and each planet spinning about its own axis. The SVG logo stays
// in place until the model is ready, and stays for good without WebGL, if
// loading fails, or with reduced motion. Rendering stops while the hero is
// out of view or the page is hidden.

import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import { RoomEnvironment } from 'three/addons/environments/RoomEnvironment.js';

const SWAY = 0.6;        // radians each way (a full turn shows the flat composition edge on)
const SWAY_PERIOD = 26;  // seconds per sway
const SPIN = {           // planets' spins, radians per second
  MOON: 0.25, MARS: 0.5, EARTH: 0.6, VENUS: 0.2, MERCURY: 0.3,
  NEPTUNE: 0.7, URANUS: 0.6, SATURN: 0.8, JUPITER: 0.9,
};

const canvas = document.querySelector('.hero-3d');
const image = document.querySelector('.hero-logo');
const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

function webglAvailable() {
  try {
    const c = document.createElement('canvas');
    return !!(c.getContext('webgl2') || c.getContext('webgl'));
  } catch {
    return false;
  }
}

// the reason the SVG stays, in the console (none: the 3D logo starts)
function keepSvg(reason, error) {
  console.info(`PEPPER: the hero shows the static logo (${reason})`, error ?? '');
}

if (!canvas) keepSvg('no canvas');
else if (reduceMotion) keepSvg('reduced motion is preferred');
else if (!webglAvailable()) keepSvg('WebGL is not available');
else if (location.protocol === 'file:') keepSvg('the page is a local file: browsers do not let it load the model; serve web/ over HTTP');
else start();

function start() {
  const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, alpha: true });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  renderer.outputColorSpace = THREE.SRGBColorSpace;
  renderer.toneMapping = THREE.ACESFilmicToneMapping;

  const scene = new THREE.Scene();
  const pmrem = new THREE.PMREMGenerator(renderer);
  scene.environment = pmrem.fromScene(new RoomEnvironment(), 0.04).texture;  // the chrome's reflections
  const sun = new THREE.DirectionalLight(0xfff4e0, 2.2);
  sun.position.set(-4, 5, 7);
  scene.add(sun);
  scene.add(new THREE.AmbientLight(0x8090ff, 0.25));

  // the logo's circle is 10 units across (1000 pixels): fit it in the view
  const camera = new THREE.PerspectiveCamera(34, 1, 0.1, 100);
  camera.position.set(0, 0, 15);

  const pivot = new THREE.Group();
  scene.add(pivot);
  pivot.add(stars());

  const spinning = [];
  new GLTFLoader().load('pepper_logo.glb', gltf => {
    gltf.scene.traverse(node => {
      if (SPIN[node.name] !== undefined) spinning.push(node);
    });
    pivot.add(gltf.scene);
    resize();
    renderer.render(scene, camera);
    canvas.classList.add('ready');      // fade in over the SVG
    image.classList.add('replaced');
    observe();
  }, undefined, error => keepSvg('the model could not be loaded', error));

  function resize() {
    const w = canvas.clientWidth, h = canvas.clientHeight;
    if (w && h && (canvas.width !== Math.round(w * renderer.getPixelRatio()))) {
      renderer.setSize(w, h, false);
      camera.aspect = w / h;
      camera.updateProjectionMatrix();
    }
  }
  window.addEventListener('resize', resize);

  const clock = new THREE.Clock();
  let t = 0;
  function frame() {
    const dt = Math.min(clock.getDelta(), 0.1);
    t += dt;
    pivot.rotation.y = SWAY * Math.sin((2 * Math.PI * t) / SWAY_PERIOD);
    pivot.rotation.x = 0.08 * Math.sin((2 * Math.PI * t) / (SWAY_PERIOD * 1.7));
    for (const p of spinning) p.rotateY(SPIN[p.name] * dt);  // about its own (tilted) axis
    resize();
    renderer.render(scene, camera);
  }

  // animate only while the hero is visible and the page is shown
  let visible = false;
  function update() {
    const run = visible && !document.hidden;
    renderer.setAnimationLoop(run ? frame : null);
    if (run) clock.getDelta();  // no jump after a pause
  }
  function observe() {
    new IntersectionObserver(entries => {
      visible = entries[0].isIntersecting;
      update();
    }).observe(canvas);
    document.addEventListener('visibilitychange', update);
  }
}

// a few stars behind the scene (they sway with it: a little parallax)
function stars() {
  const rng = mulberry32(7);
  const n = 160, pos = new Float32Array(3 * n);
  for (let i = 0; i < n; i++) {
    pos[3 * i] = (rng() - 0.5) * 22;
    pos[3 * i + 1] = (rng() - 0.5) * 22;
    pos[3 * i + 2] = -4 - rng() * 6;
  }
  const g = new THREE.BufferGeometry();
  g.setAttribute('position', new THREE.BufferAttribute(pos, 3));
  return new THREE.Points(g, new THREE.PointsMaterial({
    color: 0xffffff, size: 0.05, transparent: true, opacity: 0.8, depthWrite: false,
  }));
}

function mulberry32(a) {
  return () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
