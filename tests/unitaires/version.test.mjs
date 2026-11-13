// Tests unitaires de outils/version.mjs (calcul de version, notes de NEWS.md).
import { test } from "node:test";
import assert from "node:assert/strict";
import { suivante, notes, moisCourant, normaliserMois } from "../../outils/version.mjs";

// Mêmes cas que .github/scripts/test-version-calendaire.sh de marculus.
test("suivante : version calendaire AAAA.M.N, compteur du mois", () => {
  assert.equal(suivante("0.9.3", [], "2026.11"), "2026.11.1", "aucune étiquette");
  assert.equal(suivante("0.9.3", ["v0.9.3", "v0.1.0"], "2026.11"), "2026.11.1", "premier après des v0.*");
  assert.equal(suivante("2026.11.1", ["v0.9.3", "v2026.11.1"], "2026.11"), "2026.11.2");
  const neuf = ["v2026.11.1", "v2026.11.2", "v2026.11.3", "v2026.11.9"];
  assert.equal(suivante("2026.11.9", neuf, "2026.11"), "2026.11.10", "tri numérique");
  assert.equal(suivante("2026.11.10", [...neuf, "v2026.11.10"], "2026.11"), "2026.11.11");
  assert.equal(suivante("2026.11.10", [...neuf, "v2026.11.10"], "2026.12"), "2026.12.1", "mois suivant");
  assert.equal(suivante("2026.11.3", ["v2026.10.30", "v2026.11.3"], "2026.11"), "2026.11.4", "autre mois sans effet");
  assert.equal(suivante("2026.11.3", ["v2026.10.30", "v2026.11.3"], "2026.10"), "2026.10.31", "mois précédent séparé");
  assert.equal(suivante("2026.11.3", ["v2026.11.3", "v2026.11.5-rc1"], "2026.11"), "2026.11.4", "non numérique ignoré");
  assert.equal(suivante("2026.11.4", ["v2026.11.3"], "2026.11"), "2026.11.5", "version courante sans étiquette");
});

test("suivante : mois sans zéro initial, valide pour semver et R", () => {
  assert.equal(suivante("2026.12.4", [], "2027.01"), "2027.1.1");
  assert.equal(suivante("2027.1.1", ["v2027.1.1", "v2027.01.2"], "2027.1"), "2027.1.3", "étiquette 01 comptée");
  assert.equal(normaliserMois("2026.09"), "2026.9");
  assert.equal(normaliserMois("2026.13"), undefined);
  assert.equal(normaliserMois("2026-11"), undefined);
  assert.throws(() => suivante("0.9.3", [], "2026-11"), /mois invalide/);
});

test("moisCourant : année et mois UTC", () => {
  assert.equal(moisCourant(new Date("2026-11-30T23:30:00-02:00")), "2026.12");
  assert.equal(moisCourant(new Date("2027-01-05T12:00:00Z")), "2027.1");
  assert.match(moisCourant(), /^\d{4}\.([1-9]|1[0-2])$/);
});

test("notes extrait la section d'une version, sans attraper un numéro plus long", () => {
  const news = [
    "# nemetonmcp 0.1.10 (2026-11-01)", "", "- Dix.", "",
    "# nemetonmcp 0.1.1 (2026-10-07)", "", "- Un.", "- Deux.", "",
    "# nemetonmcp 0.1.0 (2026-10-06)", "", "- Zéro."
  ].join("\n");
  assert.equal(notes(news, "0.1.1"), "- Un.\n- Deux.");
  assert.equal(notes(news, "0.1.0"), "- Zéro.");
  assert.equal(notes(news, "0.2.0"), "");
});
