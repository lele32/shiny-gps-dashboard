# Compatibility entrypoint for the former beta launcher.
# The production app and beta now use the same modular runtime so fixes do not
# drift between two copies of the GPS workspace.

source("app.R", local = environment())
