# Same base image and digest as .devcontainer/devcontainer.json so dev and prod
# run the same artifact. Re-pin both files together when bumping the R version.
FROM rocker/shiny@sha256:0b7b6726e32e99b1daef48dc7bdce6963a53e419380762fd0070789f3fc1572e

WORKDIR /srv/app

# System libraries that R package binaries dynamically link against. PPM gives
# us .so files but not the libs they need at load time. Add new entries here
# as `renv::restore()` surfaces them — each one corresponds to a real
# "cannot open shared object file" error from a specific package:
#   - libuv1   → fs (>= 2.x) needs libuv.so.1
RUN apt-get update && apt-get install -y --no-install-recommends \
      libuv1 \
    && rm -rf /var/lib/apt/lists/*

# Layer 1: renv bootstrap. Re-runs only when renv.lock or activate.R changes,
# so app-code edits don't trigger the 31-package reinstall.
COPY .Rprofile ./
COPY renv/activate.R renv/settings.json ./renv/
COPY renv.lock ./
RUN R --no-save -e "renv::restore(prompt = FALSE)"

# Layer 2: app code. Changes more often than the lockfile.
COPY app.R ./
COPY R/ ./R/

EXPOSE 3838

# Override rocker/shiny's default shiny-server CMD. We run a single R process
# serving app.R directly; Caddy handles TLS and routing in front of us.
# --no-save: don't write .RData on container stop.
CMD ["R", "--no-save", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
