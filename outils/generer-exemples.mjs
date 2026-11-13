// Génère les données FICTIVES des vues (aucune donnée réelle) :
//   clients/claude/vues/exemples/selection/{selection.json, commune.geojson, sections.geojson, parcelles.geojson}
//   clients/claude/vues/exemples/atlas/{atlas.geojson, contexte.geojson}
//   clients/claude/vues/exemples/calcul/calcul.json
//   clients/claude/vues/exemples/plan/{plan.json, plan.geojson, contexte.geojson}
//   clients/claude/vues/exemples/rasters/{rasters.json, ugs.geojson, contexte.geojson, rasters/*.png}
// Le catalogue des familles (clients/claude/vues/exemples/catalogue.json) est celui du cœur
// nemeton (R/indicator-config.R) ; les valeurs, elles, sont tirées au hasard.
import { readFileSync, writeFileSync, mkdirSync, copyFileSync } from "node:fs";
import { deflateSync } from "node:zlib";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const ici = dirname(fileURLToPath(import.meta.url));
const ex = join(ici, "../clients/claude/vues/exemples");

let graine = 20261006;
const alea = () => ((graine = (graine * 1664525 + 1013904223) >>> 0) / 4294967296);
const r6 = (x) => Math.round(x * 1e6) / 1e6;

// Réseau de sommets légèrement déformé : les parcelles voisines partagent leurs bords.
const LON0 = 4.895, LAT0 = 47.300, NX = 24, NY = 16, DX = 0.0024, DY = 0.0016;
const sommets = [];
for (let j = 0; j <= NY; j++) {
  sommets.push([]);
  for (let i = 0; i <= NX; i++) {
    const bord = i === 0 || j === 0 || i === NX || j === NY;
    sommets[j].push([
      r6(LON0 + i * DX + (bord ? 0 : (alea() - 0.5) * DX * 0.55)),
      r6(LAT0 + j * DY + (bord ? 0 : (alea() - 0.5) * DY * 0.55))
    ]);
  }
}
const M_PAR_DEG_LAT = 111320, M_PAR_DEG_LON = 111320 * Math.cos((LAT0 * Math.PI) / 180);
function aire(anneau) {
  let s = 0;
  for (let k = 0; k < anneau.length - 1; k++) {
    s += anneau[k][0] * M_PAR_DEG_LON * anneau[k + 1][1] * M_PAR_DEG_LAT -
         anneau[k + 1][0] * M_PAR_DEG_LON * anneau[k][1] * M_PAR_DEG_LAT;
  }
  return Math.abs(s / 2);
}

const section = (i, j) => (j < NY / 2 ? (i < NX / 2 ? "A" : "B") : (i < NX / 2 ? "C" : "D"));
// Massif boisé : une tache elliptique à cheval sur B et D.
const enForet = (i, j) => ((i - 17) / 6.5) ** 2 + ((j - 8.5) / 5.5) ** 2 < 1;

const INSEE = "99999";
const compteurs = {};
const parcelles = [];
for (let j = 0; j < NY; j++) {
  for (let i = 0; i < NX; i++) {
    const s = section(i, j);
    compteurs[s] = (compteurs[s] || 0) + 1;
    const anneau = [sommets[j][i], sommets[j][i + 1], sommets[j + 1][i + 1], sommets[j + 1][i], sommets[j][i]];
    const numero = String(compteurs[s]);
    parcelles.push({
      type: "Feature",
      properties: {
        idu: INSEE + "000" + s.padStart(2, "0") + numero.padStart(4, "0"),
        section: s, numero, contenance: Math.round(aire(anneau)),
        foret: enForet(i, j) ? 1 : 0
      },
      geometry: { type: "Polygon", coordinates: [anneau] },
      _ij: [i, j]
    });
  }
}

const fc = (features) => ({ type: "FeatureCollection", features });
const contour = (i0, j0, i1, j1) => {
  const a = [];
  for (let i = i0; i <= i1; i++) a.push(sommets[j0][i]);
  for (let j = j0 + 1; j <= j1; j++) a.push(sommets[j][i1]);
  for (let i = i1 - 1; i >= i0; i--) a.push(sommets[j1][i]);
  for (let j = j1 - 1; j >= j0; j--) a.push(sommets[j][i0]);
  return [a];
};
const H = NX / 2, V = NY / 2;
const sections = fc([
  ["A", 0, 0, H, V], ["B", H, 0, NX, V], ["C", 0, V, H, NY], ["D", H, V, NX, NY]
].map(([s, i0, j0, i1, j1]) => ({ type: "Feature", properties: { section: s },
  geometry: { type: "Polygon", coordinates: contour(i0, j0, i1, j1) } })));
const commune = fc([{ type: "Feature", properties: { nom: "Commune fictive" },
  geometry: { type: "Polygon", coordinates: contour(0, 0, NX, NY) } }]);

const sel = join(ex, "selection");
mkdirSync(sel, { recursive: true });
const propres = parcelles.map(({ _ij, ...f }) => f);
writeFileSync(join(sel, "parcelles.geojson"), JSON.stringify(fc(propres)));
writeFileSync(join(sel, "sections.geojson"), JSON.stringify(sections));
writeFileSync(join(sel, "commune.geojson"), JSON.stringify(commune));
const resume = {};
for (const p of propres) {
  const r = (resume[p.properties.section] ||= { section: p.properties.section, n: 0, surface_ha: 0 });
  r.n++; r.surface_ha += p.properties.contenance / 1e4;
}
writeFileSync(join(sel, "selection.json"), JSON.stringify({
  exemple: true,
  insee: INSEE, commune: "Commune fictive", departement: "99",
  n: propres.length,
  surface_ha: Math.round(propres.reduce((s, p) => s + p.properties.contenance, 0) / 100) / 100,
  mode: "complet", bd_foret: true,
  sections: Object.values(resume).map((r) => ({ ...r, surface_ha: Math.round(r.surface_ha * 100) / 100 })),
  nom_projet: "Forêt exemple",
  source: "Données fictives générées par outils/generer-exemples.mjs"
}, null, 1));

// ---------------------------------------------------------------- Atlas
// Une UG par paire de parcelles boisées voisines (horizontalement).
const catalogue = JSON.parse(readFileSync(join(ex, "catalogue.json"), "utf8"));
const boisees = parcelles.filter((p) => enForet(...p._ij));
const parIj = new Map(boisees.map((p) => [p._ij.join(","), p]));
const pris = new Set();
const ugs = [];
for (const p of boisees) {
  const [i, j] = p._ij;
  if (pris.has(p)) continue;
  const voisin = parIj.get(i + 1 + "," + j);
  const membres = voisin && !pris.has(voisin) ? [p, voisin] : [p];
  membres.forEach((m) => pris.add(m));
  const i1 = i + membres.length;
  ugs.push({ membres, i, j, anneau: contour(i, j, i1, j + 1) });
}

const groupes = ["Futaie régulière", "Futaie irrégulière", "Taillis sous futaie", "Îlot de sénescence"];
const borne = (x) => Math.max(0, Math.min(100, x));
const features = ugs.map((u, k) => {
  // Comme le connecteur : une UGF d'une seule parcelle garde le nom par
  // défaut de nemetonshiny (sa référence cadastrale) et s'affiche « UGF n » ;
  // les autres portent un nom choisi.
  const refs = u.membres.map((m) => m.properties.idu);
  const courte = (idu) => idu.slice(8, 10).replace(/^0+/, "") + " " + Number(idu.slice(10));
  const defaut = refs.length === 1;
  const props = {
    ug_id: "ug_" + (k + 1),
    label: defaut ? "UGF " + (k + 1) : "UG " + (k + 1),
    groupe: groupes[(u.i + u.j) % groupes.length],
    surface_ha: Math.round(u.membres.reduce((s, m) => s + m.properties.contenance, 0) / 100) / 100,
    label_cadastre: defaut ? refs[0] : "UG " + (k + 1),
    label_par_defaut: defaut,
    parcelles: refs.map(courte).join(", "),
    n_parcelles: refs.length
  };
  // Gradients spatiaux pour que les cartes racontent quelque chose.
  const x = (u.i - 11) / 12, y = (u.j - 3) / 11;
  for (const fam of catalogue) {
    const notes = [];
    for (const ind of fam.indicateurs) {
      let v = borne(50 + 30 * Math.sin(3 * x + fam.code.charCodeAt(0)) * Math.cos(2 * y) + (alea() - 0.5) * 30);
      let statut = null;
      if (ind.code === "A5") { v = null; statut = "hors_zone_urbaine"; }
      if (ind.code === "R5" && alea() < 0.6) { v = null; statut = "hors_zone_validite"; }
      if (ind.code === "R7" && alea() < 0.15) { v = null; statut = "donnee_climat_absente"; }
      props[ind.colonne_norm] = v == null ? null : Math.round(v * 10) / 10;
      if (statut) props[ind.statut] = statut;
      if (v != null) notes.push(v);
    }
    const col = { C: "famille_carbone", B: "famille_biodiversite", W: "famille_eau", A: "famille_air",
      F: "famille_sol", L: "famille_paysage", T: "famille_temporel", R: "famille_risque",
      S: "famille_social", P: "famille_production", E: "famille_energie", N: "famille_naturalite" }[fam.code];
    fam.colonne = col;
    props[col] = notes.length ? Math.round((notes.reduce((a, b) => a + b, 0) / notes.length) * 10) / 10 : null;
  }
  // Production et biodiversité s'opposent : de quoi montrer la carte bivariée.
  props.famille_production = Math.round(borne(30 + 60 * (x + 1) / 2 + (alea() - 0.5) * 20) * 10) / 10;
  props.famille_biodiversite = Math.round(borne(100 - props.famille_production + (alea() - 0.5) * 25) * 10) / 10;
  return { type: "Feature", properties: props, geometry: { type: "Polygon", coordinates: u.anneau } };
});

const moyenne = (col) => {
  const v = features.map((f) => f.properties[col]).filter((x) => x != null);
  return v.length ? Math.round((v.reduce((a, b) => a + b, 0) / v.length) * 10) / 10 : null;
};
const familles = catalogue.map((f) => ({ code: f.code, nom: f.nom_fr, score: moyenne(f.colonne) }));
const atlas = {
  type: "FeatureCollection",
  nemeton: {
    exemple: true,
    project_id: "20261006_120000_demo",
    name: "Forêt exemple (données fictives)",
    global_score: Math.round((familles.reduce((s, f) => s + (f.score || 0), 0) / familles.length) * 10) / 10,
    ndp_level: 2, ndp_name: "Diagnostic intermédiaire",
    confidence: 0.72,
    updated_at: "2026-10-06T12:00:00",
    langue: "fr",
    n_ugf: features.length, n_parcelles: boisees.length,
    familles,
    genere_a: "2026-10-06T12:00:00+0200",
    catalogue
  },
  features
};
const at = join(ex, "atlas");
mkdirSync(at, { recursive: true });
writeFileSync(join(at, "atlas.geojson"), JSON.stringify(atlas));

// Contexte : une route en travers et un ruisseau sinueux.
const ligne = (f) => Array.from({ length: 40 }, (_, k) => f(k / 39)).map(([a, b]) => [r6(a), r6(b)]);
writeFileSync(join(at, "contexte.geojson"), JSON.stringify(fc([
  { type: "Feature", properties: { couche: "route", nature: "Route à 1 chaussée", importance: "3", nom: "D 905" },
    geometry: { type: "LineString", coordinates: ligne((t) => [LON0 + t * NX * DX, LAT0 + DY * (3 + 6 * t)]) } },
  { type: "Feature", properties: { couche: "cours_eau", nature: "Ruisseau", nom: "Ruisseau fictif" },
    geometry: { type: "LineString", coordinates: ligne((t) => [LON0 + DX * (14 + 3 * Math.sin(t * 7)), LAT0 + t * NY * DY]) } }
])));
// Plan d'actions : les UG de l'Atlas, et un plan type sur 20 ans.
const pl = join(ex, "plan");
mkdirSync(pl, { recursive: true });
const familleCols = ["ug_id", "label", "label_cadastre", "label_par_defaut", "parcelles", "n_parcelles", "groupe", "surface_ha", "famille_carbone", "famille_biodiversite", "famille_eau",
  "famille_air", "famille_sol", "famille_paysage", "famille_temporel", "famille_risque", "famille_social",
  "famille_production", "famille_energie", "famille_naturalite"];
writeFileSync(join(pl, "plan.geojson"), JSON.stringify({
  type: "FeatureCollection",
  nemeton: { exemple: true, project_id: atlas.nemeton.project_id, name: atlas.nemeton.name, genere_a: "2026-10-06T12:00:00+0200" },
  features: features.map((f) => ({ type: "Feature", geometry: f.geometry,
    properties: Object.fromEntries(familleCols.map((c) => [c, f.properties[c] ?? null])) }))
}));
// Même fond de contexte que l'Atlas, écrit plus bas.
const BASE = 2026;
const modeles = [
  ["eclaircie", "moderee", 2, "haute", "planifiee", ["P", "C"], { volume_m3: 85, surface_ha: 3.1, cout_eur: 900, revenu_eur: 4200 }],
  ["depressage", null, 1, "moyenne", "validee", ["P"], { surface_ha: 2.4, cout_eur: 1800 }],
  ["cloisonnement", null, 1, "moyenne", "realisee", ["P", "F"], { surface_ha: 4, cout_eur: 1200 }],
  ["regeneration", null, 6, "moyenne", "proposee", ["B", "T"], { surface_ha: 2 }],
  ["observation", null, 3, "basse", "planifiee", ["B"], {}],
  ["coupe_rase", "forte", 14, "basse", "abandonnee", ["P"], { volume_m3: 320, revenu_eur: 19000 }],
  ["plantation", null, 15, "moyenne", "proposee", ["C", "P"], { surface_ha: 2.8, nb_tiges: 4500, cout_eur: 9800 }],
  ["protection", null, 2, "haute", "validee", ["B", "N"], { cout_eur: 650 }],
  ["eclaircie", "faible", 9, "moyenne", "proposee", ["P", "R"], { volume_m3: 60, revenu_eur: 2600 }],
  ["entretien", null, 4, "basse", "planifiee", ["S", "L"], { cout_eur: 400 }],
  ["autre", null, 5, "moyenne", "proposee", ["W", "B"], { cout_eur: 2500 }]
];
const actionsPlan = [], historique = [];
modeles.forEach((m, k) => {
  const u = features[(k * 7 + 3) % features.length].properties;
  const id = "act_20261006" + String(100000 + k) + "_demo" + k;
  const claude = m[4] === "proposee" && (k === 6 || k === 8);
  const a = { id, ug_id: u.ug_id, type: m[0], type_libre: m[0] === "autre" ? "Création d'une mare forestière" : null,
    intensite: m[1], annee_cible: m[2], annee: BASE + m[2], duree: 1, priorite: m[3], statut: m[4], objectifs_lies: m[5],
    quantite: m[6], source: { origine: claude ? "claude" : "vue", extrait_texte: claude ? "Biodiversité (B) faible et régénération lente : favoriser le mélange." : null },
    commentaire: null, cree_par: "gestionnaire", cree_le: "2026-09-1" + (k % 9) + "T09:00:00+0200",
    ug_label: u.label, propose_par_claude: claude, version: "v" + k };
  actionsPlan.push(a);
  historique.push({ ts: a.cree_le, user: "gestionnaire", action_id: id, op: "create" });
  if (m[4] !== "proposee") historique.push({ ts: "2026-10-01T14:3" + (k % 10) + ":00+0200", user: "gestionnaire", action_id: id, op: "update", champ: "statut", ancien: "proposee", nouveau: m[4] === "realisee" ? "planifiee" : m[4] });
});
writeFileSync(join(pl, "plan.json"), JSON.stringify({
  exemple: true, ok: true, projet: atlas.nemeton.project_id, nom: atlas.nemeton.name,
  horizon_annees: 20, annee_base: BASE, actions: actionsPlan, historique: historique.reverse(),
  commentaires_ug: { [features[3].properties.ug_id]: "Peuplement fragilisé par les sécheresses de 2022." },
  catalogue: {
    types: ["coupe_rase", "eclaircie", "depressage", "plantation", "regeneration", "cloisonnement", "desserte", "observation", "protection", "entretien", "autre"],
    statuts: ["proposee", "validee", "planifiee", "realisee", "abandonnee"], priorites: ["haute", "moyenne", "basse"],
    familles: ["C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N"],
    transitions: { proposee: ["validee", "abandonnee"], validee: ["planifiee", "abandonnee"], planifiee: ["validee", "realisee", "abandonnee"], realisee: [], abandonnee: ["proposee"] },
    types_marculus: ["coupe_rase", "eclaircie", "depressage", "observation"]
  },
  peut_ecrire: false, genere_a: "2026-10-06T12:00:00+0200"
}, null, 1) + "\n");

// Calcul : état figé d'un calcul en cours, sans connecteur.
const cal = join(ex, "calcul");
mkdirSync(cal, { recursive: true });
writeFileSync(join(cal, "calcul.json"), JSON.stringify({
  exemple: true, projet: "20261006_101500_exmp", nom: "Forêt fictive de démonstration",
  etat: {
    ok: true, projet: "20261006_101500_exmp", statut: "en_cours", job_id: "20261006101502-abcd",
    phase: "computing", progression: 12, progression_max: 41, indicateurs_faits: 12, indicateurs_total: 41,
    tache: "B2 — Diversité structurale", ecoule_s: 1694
  }
}, null, 1) + "\n");
copyFileSync(join(at, "contexte.geojson"), join(pl, "contexte.geojson"));

// ---------------------------------------------------------------- Rasters
// Images fictives (santé, accessibilité, climat) sur l'emprise des UG, au
// format de vue_rasters : rasters.json, ugs.geojson et rasters/*.png.
const crcTable = Array.from({ length: 256 }, (_, n) => {
  let c = n;
  for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
  return c >>> 0;
});
const crc32 = (buf) => { let c = 0xffffffff; for (const b of buf) c = crcTable[(c ^ b) & 0xff] ^ (c >>> 8); return (c ^ 0xffffffff) >>> 0; };
function png(l, h, pixel) {
  const brut = Buffer.alloc((l * 4 + 1) * h);
  for (let y = 0; y < h; y++) {
    brut[y * (l * 4 + 1)] = 0;
    for (let x = 0; x < l; x++) {
      const c = pixel(x, y), o = y * (l * 4 + 1) + 1 + x * 4;
      if (!c) continue;
      brut[o] = parseInt(c.slice(1, 3), 16); brut[o + 1] = parseInt(c.slice(3, 5), 16); brut[o + 2] = parseInt(c.slice(5, 7), 16); brut[o + 3] = 255;
    }
  }
  const bloc = (type, data) => {
    const t = Buffer.concat([Buffer.from(type), data]);
    const lg = Buffer.alloc(4); lg.writeUInt32BE(data.length);
    const crc = Buffer.alloc(4); crc.writeUInt32BE(crc32(t));
    return Buffer.concat([lg, t, crc]);
  };
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(l, 0); ihdr.writeUInt32BE(h, 4); ihdr[8] = 8; ihdr[9] = 6;
  return Buffer.concat([Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]), bloc("IHDR", ihdr),
    bloc("IDAT", deflateSync(brut)), bloc("IEND", Buffer.alloc(0))]);
}

const I0 = 9, I1 = 25, J0 = 2, J1 = 15, PX = 6;           // emprise en cases de parcelles, pixels par case
const LARG = (I1 - I0) * PX, HAUT = (J1 - J0) * PX;
const emprise = [[r6(LAT0 + J0 * DY), r6(LON0 + I0 * DX)], [r6(LAT0 + J1 * DY), r6(LON0 + I1 * DX)]];
const ij = (x, y) => [I0 + (x + 0.5) / PX, J1 - (y + 0.5) / PX];    // ligne 0 = nord
const dansForet = (i, j) => enForet(i - 0.5, j - 0.5);
// Foyer de dépérissement qui s'étend avec le temps.
const foyer = (i, j, rayon) => {
  const d = Math.hypot(i - 19, (j - 6) * 1.3);
  return d < rayon * 0.4 ? 3 : d < rayon * 0.7 ? 2 : d < rayon ? 1 : 0;
};
const COUL_SEV = ["#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222"];
const RAMPE_SCORE = ["#006837", "#1a9850", "#66bd63", "#a6d96a", "#d9ef8b", "#fee08b", "#fdae61", "#f46d43", "#d73027"];
const BIV = ["#e8e8e8", "#e4acac", "#c85a5a", "#b0d5df", "#ad9ea5", "#985356", "#64acbe", "#627f8c", "#574249"];
const rampe = (stops, t) => stops[Math.max(0, Math.min(stops.length - 1, Math.round(t * (stops.length - 1))))];
const valeurs = {
  fordead_severite: (i, j, r = 2.8) => (dansForet(i, j) ? foyer(i, j, r) : null),
  reconfort_score: (i, j) => (dansForet(i, j) ? 50 + 35 * Math.sin(i / 2.2) * Math.cos(j / 3) : null),
  acc_skidder: (i, j) => (dansForet(i, j) ? (Math.abs(j - 9) < 2 ? 1 : Math.abs(j - 9) < 4 ? 2 : 3) : 4),
  climat_eobs: (i, j) => 1 + Math.min(2, Math.floor((J1 - j) / ((J1 - J0) / 3))) * 3 + Math.min(2, Math.floor((i - I0) / ((I1 - I0) / 3)))
};
const couleur = {
  fordead_severite: (v) => (v ? COUL_SEV[v] : null),
  reconfort_score: (v) => (v == null ? null : rampe(RAMPE_SCORE, (v - 1) / 99)),
  acc_skidder: (v) => ({ 1: "#2E7D32", 2: "#9CCC65", 3: "#C62828" })[v] || null,
  climat_eobs: (v) => BIV[v - 1]
};
const rs = join(ex, "rasters");
mkdirSync(join(rs, "rasters"), { recursive: true });
const ecrireImage = (nom, f, c) => {
  writeFileSync(join(rs, "rasters", nom + ".png"), png(LARG, HAUT, (x, y) => c(f(...ij(x, y)))));
  return "rasters/" + nom + ".png";
};
// Statistiques par UG : échantillon de points dans chaque UG (rectangle de cases).
const points = (u) => {
  const n = u.membres.length, pts = [];
  for (let a = 0; a < 4 * n; a++) for (let b = 0; b < 4; b++) pts.push([u.i + (a + 0.5) / 4, u.j + 1 - (b + 0.5) / 4]);
  return pts;
};
const statsClasses = (f) => Object.fromEntries(ugs.map((u, k) => {
  const v = points(u).map(([i, j]) => f(i, j)).filter((x) => x != null);
  const parts = {};
  v.forEach((x) => { parts[x] = (parts[x] || 0) + 1 / v.length; });
  Object.keys(parts).forEach((x) => { parts[x] = Math.round(parts[x] * 1e4) / 1e4; });
  return ["ug_" + (k + 1), { parts, surface_ha: features[k].properties.surface_ha }];
}));
const statsContinues = (f) => Object.fromEntries(ugs.map((u, k) => {
  const v = points(u).map(([i, j]) => f(i, j)).filter((x) => x != null);
  return ["ug_" + (k + 1), { moyenne: Math.round(v.reduce((a, b) => a + b, 0) / v.length * 10) / 10,
    min: Math.round(Math.min(...v) * 10) / 10, max: Math.round(Math.max(...v) * 10) / 10, n_px: v.length }];
}));
const datesFrise = [["2025-12-31", 1.4], ["2026-06-30", 2.1], ["2026-09-25", 2.8]];
const couches = [
  {
    id: "fordead_severite", theme: "sante", libelle: "FORDEAD · sévérité du dépérissement",
    source: "FORDEAD (nemeton, Sentinel-2)", date: null,
    legende: { type: "classes", classes: [
      { valeur: 0, libelle: "sain" }, { valeur: 1, libelle: "faible", couleur: COUL_SEV[1] },
      { valeur: 2, libelle: "moyenne", couleur: COUL_SEV[2] }, { valeur: 3, libelle: "forte", couleur: COUL_SEV[3] },
      { valeur: 4, libelle: "sol nu", couleur: COUL_SEV[4] }] },
    image: ecrireImage("fordead_severite", valeurs.fordead_severite, couleur.fordead_severite),
    emprise, largeur: LARG, hauteur: HAUT,
    par_ug: statsClasses(valeurs.fordead_severite),
    frise: datesFrise.map(([d, r]) => ({ date: d,
      image: ecrireImage("fordead_severite_" + d.replace(/-/g, ""), (i, j) => valeurs.fordead_severite(i, j, r), couleur.fordead_severite) })),
    serie_ug: {
      dates: datesFrise.map(([d]) => d),
      surfaces_ha: Object.fromEntries(ugs.map((u, k) => ["ug_" + (k + 1), datesFrise.map(([, r]) => {
        const v = points(u).map(([i, j]) => valeurs.fordead_severite(i, j, r)).filter((x) => x != null);
        return Math.round(v.filter((x) => x >= 1).length / Math.max(1, v.length) * features[k].properties.surface_ha * 100) / 100;
      })]))
    }
  },
  {
    id: "reconfort_score", theme: "sante", libelle: "RECONFORT · score de dépérissement",
    source: "RECONFORT (nemeton, DSF)", date: "2025",
    legende: { type: "continu", min: 1, max: 100, couleurs: RAMPE_SCORE, format: "nombre" },
    image: ecrireImage("reconfort_score", valeurs.reconfort_score, couleur.reconfort_score),
    emprise, largeur: LARG, hauteur: HAUT, par_ug: statsContinues(valeurs.reconfort_score)
  },
  {
    id: "acc_skidder", theme: "accessibilite", libelle: "Débusqueur (skidder)",
    source: "foretaccess (nemetonshiny)", date: "2026-10-01",
    legende: { type: "classes", classes: [
      { valeur: 1, libelle: "Parcourable (engin)", couleur: "#2E7D32" }, { valeur: 2, libelle: "Accessible", couleur: "#9CCC65" },
      { valeur: 3, libelle: "Non accessible", couleur: "#C62828" }, { valeur: 4, libelle: "Hors forêt" }] },
    image: ecrireImage("acc_skidder", valeurs.acc_skidder, couleur.acc_skidder),
    emprise, largeur: LARG, hauteur: HAUT, par_ug: statsClasses(valeurs.acc_skidder)
  },
  {
    id: "climat_eobs", theme: "regeneration", libelle: "Climat EOBS · ΔT°max × Δprécipitations",
    source: "EOBS (nemeton, régénération)", date: "2006–2020",
    legende: { type: "bivarie", n: 3, classes: BIV.map((c, k) => ({ valeur: k + 1, couleur: c })),
      axe_x: { nom: "ΔP", unite: "mm", bornes: [-270, 120] }, axe_y: { nom: "ΔT°max", unite: "°C", bornes: [-0.7, 4] } },
    image: ecrireImage("climat_eobs", valeurs.climat_eobs, couleur.climat_eobs),
    emprise, largeur: LARG, hauteur: HAUT, par_ug: statsClasses(valeurs.climat_eobs)
  }
];
writeFileSync(join(rs, "rasters.json"), JSON.stringify({
  exemple: true, project_id: "20261006_120000_demo", name: "Forêt exemple (données fictives)",
  langue: "fr", genere_a: "2026-10-06T12:00:00+0200", theme_initial: null,
  themes: [{ id: "sante", libelle: "Santé des forêts" }, { id: "accessibilite", libelle: "Accessibilité" },
    { id: "regeneration", libelle: "Régénération" }],
  couches,
  variables_ug: [{ id: "regen_priorite", theme: "regeneration", libelle: "Indice de priorité" },
    { id: "regen_d_tmax", theme: "regeneration", libelle: "ΔT°max sous couvert (°C)" }],
  absentes: { fast: "Calculer une alerte FAST dans nemetonshiny (Suivi sanitaire)." }
}, null, 1) + "\n");
writeFileSync(join(rs, "ugs.geojson"), JSON.stringify(fc(features.map((f, k) => ({
  type: "Feature", geometry: f.geometry,
  properties: { ug_id: f.properties.ug_id, label: f.properties.label, parcelles: f.properties.parcelles,
    surface_ha: f.properties.surface_ha,
    regen_priorite: Math.round(borne(30 + 50 * Math.sin(k / 3) ** 2) * 10) / 10,
    regen_d_tmax: Math.round((-7 + 4 * ((k * 37) % 11) / 10) * 100) / 100 }
})))));
copyFileSync(join(at, "contexte.geojson"), join(rs, "contexte.geojson"));
console.log(`${propres.length} parcelles, ${features.length} UG d'exemple écrites dans clients/claude/vues/exemples/.`);
