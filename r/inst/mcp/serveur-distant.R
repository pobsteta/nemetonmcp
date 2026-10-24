# Serveur MCP nemeton distant (HTTP, authentification Keycloak), pour un
# connecteur claude.ai. Configuration par variables d'environnement : voir
# ?nemetonmcp::config_distant et deploiement/ a la racine du depot.
#
#   NEMETON_MCP_URL=https://nemeton.example.org \
#   NEMETON_KEYCLOAK_URL=https://auth.example.org/realms/nemeton \
#   NEMETON_MCP_SECRET=... Rscript serveur-distant.R

suppressPackageStartupMessages(loadNamespace("nemetonmcp"))
nemetonmcp::serveur_mcp_distant()
