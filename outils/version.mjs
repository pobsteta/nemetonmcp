// Version unique du dépôt, portée par r/DESCRIPTION et recopiée dans
// package.json, clients/claude/.claude-plugin/plugin.json et le premier titre de NEWS.md.
//
// Versions calendaires AAAA.M.N, comme marculus (vAAAA.MM.N) : année et mois
// UTC du jour de la release, N = compteur du mois, qui repart à 1 chaque mois
// (2026.11.1, 2026.11.2, … 2026.11.10, puis 2026.12.1). N vaut le plus grand
// numéro du mois parmi les étiquettes v* et la version courante, plus 1.
// Le mois s'écrit sans zéro initial (2027.1.1, pas 2027.01.1) : semver
// (package.json, plugin.json) interdit les zéros initiaux et R les retire
// (packageVersion() afficherait 2027.1.1). D'octobre à décembre, la forme est
// exactement celle de marculus. Toute version calendaire dépasse 0.9.3.
//
// Usage :
//   node outils/version.mjs                    affiche la version
//   node outils/version.mjs verifier           échoue si les fichiers divergent
//   node outils/version.mjs suivante [AAAA.MM] affiche la prochaine version
//                                              (défaut : mois courant, UTC)
//   node outils/version.mjs monter [X.Y.Z]     écrit la prochaine version (ou
//                                              X.Y.Z) partout et ajoute à NEWS.md
//                                              la section des commits depuis la
//                                              dernière étiquette
//   node outils/version.mjs notes [X.Y.Z]      section de NEWS.md d'une version
//
// Pas de cycle de développement X.Y.Z.9000 : package.json exige du semver.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const racine = join(dirname(fileURLToPath(import.meta.url)), "..");
const chemin = (f) => join(racine, f);
const lire = (f) => readFileSync(chemin(f), "utf8");
const FICHIERS = { description: "r/DESCRIPTION", npm: "package.json", plugin: "clients/claude/.claude-plugin/plugin.json", news: "NEWS.md" };
// Trois entiers sans zéro initial : valide pour semver comme pour R.
const SEMVER = /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)$/;
const TITRE = /^# nemetonmcp (\d+\.\d+\.\d+)/m;

function echec(message) {
  console.error(`Erreur : ${message}`);
  process.exit(1);
}

function versions() {
  const desc = lire(FICHIERS.description).match(/^Version:\s*(\S+)/m)?.[1];
  const npm = JSON.parse(lire(FICHIERS.npm)).version;
  const plugin = JSON.parse(lire(FICHIERS.plugin)).version;
  const news = existsSync(chemin(FICHIERS.news)) ? lire(FICHIERS.news).match(TITRE)?.[1] : undefined;
  return { description: desc, npm, plugin, news };
}

// Mois courant « AAAA.M », en UTC, sans zéro initial.
export function moisCourant(date = new Date()) {
  return `${date.getUTCFullYear()}.${date.getUTCMonth() + 1}`;
}

// « AAAA.MM » ou « AAAA.M » → « AAAA.M » ; undefined si invalide.
export function normaliserMois(mois) {
  const m = /^(\d{4})\.(\d{1,2})$/.exec(mois ?? "");
  if (!m || +m[2] < 1 || +m[2] > 12) return undefined;
  return `${+m[1]}.${+m[2]}`;
}

// Prochaine version du mois : le plus grand N parmi les étiquettes vAAAA.M.N
// (ou vAAAA.MM.N) du mois et la version courante, plus 1. Les autres mois, les
// anciennes versions 0.* et les étiquettes non numériques n'influent pas.
export function suivante(actuelle, etiquettes, mois = moisCourant()) {
  const cible = normaliserMois(mois);
  if (!cible) throw new Error(`mois invalide : '${mois}' (attendu AAAA.MM)`);
  const numeros = [...etiquettes.map((e) => e.replace(/^v/, "")), actuelle ?? ""]
    .map((x) => /^(\d{4})\.(\d{1,2})\.(\d+)$/.exec(x))
    .filter((m) => m && normaliserMois(`${m[1]}.${m[2]}`) === cible)
    .map((m) => Number(m[3]));
  return `${cible}.${Math.max(0, ...numeros) + 1}`;
}

function git(...args) {
  try { return execFileSync("git", args, { cwd: racine, encoding: "utf8" }).trim(); } catch { return ""; }
}

// Sujets des commits depuis la dernière étiquette vX.Y.Z, sans les commits de
// version eux-mêmes : avec la fusion « squash », un sujet = une PR.
function etiquettes() {
  return git("tag", "--list", "v[0-9]*").split("\n").filter(Boolean);
}

function nouveautes() {
  const derniere = git("describe", "--tags", "--abbrev=0", "--match", "v[0-9]*");
  const plage = derniere ? `${derniere}..HEAD` : "HEAD";
  return git("log", plage, "--no-merges", "--format=%s")
    .split("\n").map((s) => s.trim())
    .filter((s) => s && !/^Version \d+\.\d+\.\d+/.test(s));
}

function ecrireVersion(v) {
  const desc = lire(FICHIERS.description).replace(/^Version:\s*\S+/m, `Version: ${v}`);
  writeFileSync(chemin(FICHIERS.description), desc);
  // Remplacement ciblé plutôt que JSON.stringify : la mise en forme reste.
  for (const f of [FICHIERS.npm, FICHIERS.plugin]) {
    const texte = lire(f).replace(/^(\s*"version":\s*")[^"]*(")/m, `$1${v}$2`);
    if (JSON.parse(texte).version !== v) echec(`version introuvable dans ${f}`);
    writeFileSync(chemin(f), texte);
  }
}

function ajouterSection(v, lignes) {
  const date = new Date().toISOString().slice(0, 10);
  const corps = lignes.length ? lignes.map((l) => `- ${l}`).join("\n") : "- Maintenance.";
  const avant = existsSync(chemin(FICHIERS.news)) ? lire(FICHIERS.news) : "";
  writeFileSync(chemin(FICHIERS.news), `# nemetonmcp ${v} (${date})\n\n${corps}\n\n${avant}`.trimEnd() + "\n");
}

export function notes(texte, v) {
  const lignes = texte.split("\n");
  const debut = lignes.findIndex((l) => l.startsWith(`# nemetonmcp ${v} `) || l === `# nemetonmcp ${v}`);
  if (debut < 0) return "";
  const fin = lignes.findIndex((l, i) => i > debut && l.startsWith("# nemetonmcp "));
  return lignes.slice(debut + 1, fin < 0 ? undefined : fin).join("\n").trim();
}

const [commande, arg] = process.argv.slice(2);
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const v = versions();
  switch (commande) {
    case undefined:
      console.log(v.description);
      break;
    case "verifier": {
      const valeurs = Object.entries(v);
      for (const [f, x] of valeurs) console.log(`${FICHIERS[f].padEnd(27)} ${x ?? "(absente)"}`);
      if (!SEMVER.test(v.description ?? "")) echec(`version de r/DESCRIPTION non conforme X.Y.Z : ${v.description}`);
      if (valeurs.some(([, x]) => x !== v.description)) echec("versions désalignées");
      console.log(`Versions alignées : ${v.description}`);
      break;
    }
    case "suivante":
      try { console.log(suivante(v.description, etiquettes(), arg)); } catch (e) { echec(e.message); }
      break;
    case "monter": {
      if (arg && !SEMVER.test(arg)) echec(`version non conforme X.Y.Z (sans zéro initial) : ${arg}`);
      const n = arg ?? suivante(v.description, etiquettes());
      ecrireVersion(n);
      ajouterSection(n, nouveautes());
      console.log(n);
      break;
    }
    case "notes":
      console.log(notes(existsSync(chemin(FICHIERS.news)) ? lire(FICHIERS.news) : "", arg ?? v.description));
      break;
    default:
      echec(`commande inconnue : ${commande} (verifier, suivante, monter, notes)`);
  }
}
