#!/usr/bin/env bash
# One-shot bootstrap for a fresh Hetzner Ubuntu 24.04 host that will run the
# multi-app Docker stack. Idempotent — safe to re-run if something interrupted.
#
# Usage:
#   ssh root@<vps-ip>
#   # paste this script into a file or pipe it via stdin, then:
#   bash bootstrap.sh
#
# After this script completes, follow the "Next steps" printed at the end:
# generate an SSH key for the deploy user, add it as a GitHub deploy key for
# this repo, clone, and `docker compose up -d --build`.

set -euo pipefail

if [ "$(id -u)" -ne 0 ]; then
	echo "bootstrap.sh: must run as root (got uid=$(id -u))" >&2
	exit 1
fi

export DEBIAN_FRONTEND=noninteractive

# ── 1. Base system + essentials ────────────────────────────────────────────────
apt-get update
apt-get upgrade -y
apt-get install -y --no-install-recommends \
	ca-certificates curl gnupg lsb-release \
	git ufw unattended-upgrades

# ── 2. Docker CE + compose plugin from Docker's official apt repo ──────────────
# Not Ubuntu's `docker.io` — that ships an older Docker and no compose v2.
if ! command -v docker >/dev/null 2>&1; then
	install -m 0755 -d /etc/apt/keyrings
	curl -fsSL https://download.docker.com/linux/ubuntu/gpg \
		| gpg --dearmor -o /etc/apt/keyrings/docker.gpg
	chmod a+r /etc/apt/keyrings/docker.gpg
	echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" \
		> /etc/apt/sources.list.d/docker.list
	apt-get update
	apt-get install -y \
		docker-ce docker-ce-cli containerd.io \
		docker-buildx-plugin docker-compose-plugin
fi
systemctl enable --now docker

# ── 3. Firewall: only SSH + 80/443 inbound ─────────────────────────────────────
# Caddy needs 80 (ACME HTTP-01 challenge + redirect to 443) and 443 (TLS).
ufw default deny incoming
ufw default allow outgoing
ufw allow OpenSSH
ufw allow 80/tcp
ufw allow 443/tcp
ufw --force enable

# ── 4. Non-root `deploy` user with docker + sudo, sharing root's authorized_keys ──
if ! id -u deploy >/dev/null 2>&1; then
	useradd -m -s /bin/bash -G docker,sudo deploy
fi
install -d -m 700 -o deploy -g deploy /home/deploy/.ssh
if [ -f /root/.ssh/authorized_keys ]; then
	install -m 600 -o deploy -g deploy \
		/root/.ssh/authorized_keys /home/deploy/.ssh/authorized_keys
fi

# Passwordless sudo for deploy — convenient for ops, acceptable for a single-admin host.
# Comment out this block if you'd rather be prompted.
echo 'deploy ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/deploy
chmod 440 /etc/sudoers.d/deploy

# ── 5. Unattended security upgrades ────────────────────────────────────────────
cat > /etc/apt/apt.conf.d/20auto-upgrades <<'EOF'
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Unattended-Upgrade "1";
EOF

# ── 6. Done. ───────────────────────────────────────────────────────────────────
cat <<'EOF'

================================================================================
Bootstrap complete. Next steps (run AS THE deploy USER, not root):

  ssh deploy@<this-host>
  ssh-keygen -t ed25519 -C "deploy@$(hostname)" -f ~/.ssh/id_ed25519 -N ''
  cat ~/.ssh/id_ed25519.pub
  # ↑ copy the printed key, then add it at:
  #   https://github.com/odcambc/dimple-helpers/settings/keys/new
  # as a read-only Deploy Key.

  git clone git@github.com:odcambc/dimple-helpers.git ~/dimple-helpers
  cd ~/dimple-helpers/infra
  docker compose up -d --build

Then verify https://dimple-helper.odcambc.com responds (DNS must be pointing
here first — A record dimple-helper.odcambc.com → this VPS's public IP).

Optional hardening (do this only AFTER you've confirmed deploy-user SSH works):
  sudo sed -i 's/^#\?PasswordAuthentication.*/PasswordAuthentication no/' /etc/ssh/sshd_config
  sudo systemctl restart ssh
================================================================================
EOF
