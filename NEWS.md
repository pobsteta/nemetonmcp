# nemetonmcp 0.8.2 (2026-10-24)

- Ajoute l'outil fiche_projet (fiche en cache et bloc display_report) (#19)

# nemetonmcp 0.8.1 (2026-10-24)

- Document d'architecture Nemeton × LLM (#17)

# nemetonmcp 0.8.0 (2026-10-24)

- Le paquet R nemetonclaude devient nemetonmcp (dépôt pobsteta/nemetonmcp) :
  `remotes::install_github("pobsteta/nemetonmcp", subdir = "r")`, serveur
  lancé par `nemetonmcp::serveur_mcp()` ou `nemetonmcp::serveur_mcp_distant()`.
- Variables d'environnement `NEMETON_CLAUDE_*` renommées `NEMETON_MCP_*`.
  Les anciennes sont encore lues, avec un avertissement, quand les nouvelles
  manquent.
- Classes d'erreur `nemetonclaude_*` renommées `nemetonmcp_*`, options R
  `nemetonclaude.*` renommées `nemetonmcp.*`, cache des vues sous
  `~/.cache/R/nemetonmcp/`. La vue Plan d'actions reconnaît les deux noms du
  conflit ; une vue Plan publiée avant 0.8.0 doit être republiée.
- Connecteur distant : `NEMETON_MCP_AUDIENCE` accepte une liste d'audiences
  (un jeton passe s'il en porte une), pour migrer de `nemeton-claude` vers
  `nemeton-mcp` sans coupure. Le modèle de client Keycloak porte les deux
  mappeurs ; l'audience par défaut reste `nemeton-claude`.
- Déploiement : `nemetonmcp.service` et `/etc/nemetonmcp.env` remplacent
  `nemeton-claude.service` et `/etc/nemeton-claude.env`.
- Le plugin Claude Code, ses vues et son skill sont rangés sous
  `clients/claude/`. La marketplace s'appelle `nemetonmcp` :
  `claude plugin marketplace add pobsteta/nemetonmcp`, puis
  `claude plugin install nemeton@nemetonmcp`. Le serveur MCP et le plugin
  gardent le nom `nemeton`.

# nemetonmcp 0.7.2 (2026-10-24)

- Rename project from nemetonClaude to nemetonmcp

# nemetonmcp 0.7.1 (2026-10-12)

- Atlas : calcule les notes des sous-indicateurs quand nemetonshiny ne les enregistre pas (#16)

# nemetonmcp 0.7.0 (2026-10-12)

- Atlas : relit les résultats au connecteur à l'ouverture, sans republication (#15)
- Skill nemeton-vues : réécrire le <title> de la copie avant de publier (#14)

# nemetonmcp 0.6.0 (2026-10-12)

- Déclare le dépôt comme marketplace Claude Code (installation durable du plugin) (#13)

# nemetonmcp 0.5.0 (2026-10-10)

- Expose croiser_onf et appliquer_ugf (UGF depuis le parcellaire ONF) (#12)
- Retire un fichier de diagnostic testthat commité par erreur (#11)

# nemetonmcp 0.4.0 (2026-10-07)

- Propose le GeoPackage au téléchargement dans l'Atlas, comme le CSV (#10)

# nemetonmcp 0.3.1 (2026-10-07)

- Affiche « UGF n » pour les UGF qui gardent leur nom cadastral (#7)

# nemetonmcp 0.3.0 (2026-10-07)

- Lot 3 : plan d'actions partagé (#6)

# nemetonmcp 0.2.0 (2026-10-06)

- Lot 2 : connecteur distant (HTTP, Keycloak) et vue Calcul (#5)
- Passe les actions GitHub en Node.js 24 et fixe Ubuntu 24.04 (#4)

# nemetonmcp 0.1.0 (2026-10-06)

Première version : lot 1 du brief `specs/BRIEF-visualisation-nemeton-claude.md`.

- Connecteur MCP étendu : les 8 outils de nemetonshiny, plus
  `chercher_commune`, `parcelles_commune`, `creer_projet`, `vue_atlas`,
  `contexte_carto` et `detail_indicateur`.
- Moteur commun des vues (`nemeton-view.js`, `nemeton-view.css`), vue
  Sélection des parcelles et vue Atlas du projet.
- Skill du plugin `nemeton-vues`.
- `outils/essayer.sh` pour tout essayer d'un coup.
- Intégration continue (tests R, `R CMD check`, tests unitaires et de bout en
  bout des vues, couverture), versions alignées et publication automatique des
  versions.
