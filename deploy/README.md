# Serving the IPM app from your own server

These files run the IPM web app in Docker on any Linux server: a Proxmox VM or LXC, an OMV host with the Compose plugin, or a cloud VM. The image is built from this repository, so updating the server means rebuilding it from a newer release.

```
deploy/
  docker/Dockerfile          image of the app: R 4.4 + packages + ipmR installed from this repository
  docker/docker-compose.yml  OPTION A: one shared app container + Caddy (HTTPS, password)
  docker/Caddyfile           reverse-proxy settings for option A
  shinyproxy/                OPTION B: ShinyProxy, one private container per user
  omv/ipm-compose.yml        compose file for the OpenMediaVault Compose plugin
  omv/update.sh              build or update the image straight from GitHub (any Docker host)
  shinyapps/                 stand-alone bundle for shinyapps.io (also attached to every release)
```

## Which option?

|                    | Option A: single container                                  | Option B: ShinyProxy (recommended for several users)                    |
|--------------------|-------------------------------------------------------------|-------------------------------------------------------------------------|
| Login              | One shared password, or one per user (Caddy basic auth)     | Users and passwords, or UNICEF/Entra ID single sign-on                  |
| Concurrent users   | They share one R process, so one person's run makes the others wait | Each user has their own R process                                 |
| Isolation of data  | Separate session folders in RAM, deleted at session end     | A separate container per user, destroyed at logout or after 30 min idle |
| Resources          | About 1-3 GB RAM in total                                   | About 1-3 GB RAM per active user                                        |
| Complexity         | Lowest                                                      | Medium (ShinyProxy needs access to the Docker socket)                   |

## 1. Prepare the host

On Proxmox, create a Debian 12 or Ubuntu 24.04 VM with 2-4 vCPU, 4-8 GB RAM and 20 GB disk, then install Docker:

```bash
curl -fsSL https://get.docker.com | sh
sudo usermod -aG docker $USER      # log out and in again
```

**Disable swap on this VM** (`sudo swapoff -a`, and remove the swap line from `/etc/fstab`). Uploaded data are kept in RAM; with swap on, they could be written to disk.

## 2. Build the app image

Either straight from GitHub (no clone needed):

```bash
curl -fsSL https://raw.githubusercontent.com/hwannis/ipmR/main/deploy/omv/update.sh -o ipm-update.sh
sudo bash ipm-update.sh            # latest release; or: sudo bash ipm-update.sh v0.4.4
```

or from a clone of the repository:

```bash
git clone https://github.com/hwannis/ipmR.git && cd ipmR
docker build -f deploy/docker/Dockerfile -t ipm-app:latest .
```

The first build takes 10-15 minutes because it installs the R packages. Later builds take less than a minute, because only ipmR itself is reinstalled. Quick test on the LAN:

```bash
docker run --rm -p 3838:3838 --tmpfs /tmp:size=2g ipm-app:latest
# open http://<server-ip>:3838 , then Ctrl+C to stop
```

## 3a. Option A: shared container with HTTPS and a password

1. Create a password hash:
   ```bash
   docker run --rm caddy:2 caddy hash-password --plaintext 'YourStrongPassword'
   ```
2. Edit `Caddyfile`:
   - replace `ipm.example.org` with your domain name;
   - paste the hash after `mics` (add one line per user if you like).
3. Start it from `deploy/docker`:
   ```bash
   docker compose up -d --build
   ```

## 3b. Option B: ShinyProxy, one private container per user

1. Build the image first (step 2).
2. Edit `shinyproxy/application.yml`: users and passwords, or the commented single-sign-on block.
3. Edit `shinyproxy/Caddyfile`: your domain name.
4. Start it:
   ```bash
   cd shinyproxy
   DOCKER_GID=$(getent group docker | cut -d: -f3) docker compose up -d
   ```

Each user logs in and gets a fresh container. The container and everything in it are deleted when the user logs out, closes the browser for 30 minutes, or presses "Delete my data now".

For UNICEF single sign-on, ask ICT for an Entra ID (Azure AD) app registration with the redirect URI `https://<your-domain>/login/oauth2/code/shinyproxy`. Then fill in tenant, client ID and secret in the commented block.

## 4. Making it reachable

Pick one:

| Access                                          | How                                                                                                                                                     | Notes                                                                                     |
|-------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|
| **VPN only** (recommended for survey microdata) | No ports opened. Users connect with the pfSense WireGuard/OpenVPN VPN, or Tailscale. In the Caddyfile use `:443` + `tls internal`, or plain `http://` on the LAN. | Nothing is exposed to the internet.                                                       |
| Public HTTPS                                    | DNS A record → your public IP. pfSense: Firewall > NAT > Port Forward TCP 80 and 443 → the VM. Caddy obtains the Let's Encrypt certificate automatically. | Keep the login on. Optionally restrict source countries with pfBlockerNG GeoIP.           |
| Cloudflare Tunnel                               | `cloudflared` container, no open ports, Cloudflare Access for login.                                                                                    | HTTPS terminates at Cloudflare, so the data pass through Cloudflare's servers in clear.   |

## 5. Data protection on your server

- Session data (uploads, results, reports) are written only to `/tmp` inside the container. That is a RAM disk (`tmpfs`) in option A, and a throw-away container in option B. Nothing is written to the host's disks or to Docker volumes.
- Data are deleted when the tab is closed, after 30 minutes of inactivity (change with `IPM_IDLE_MINUTES`), or with "Delete my data now". In option B the whole container is destroyed as well.
- Logs (`docker logs`) contain only start/stop messages and step names, never data values.
- **Proxmox backups:** use the *stop* or *snapshot* backup mode without RAM. Do not use "suspend", and do not include RAM in VM snapshots: a RAM image could contain a session in progress.
- The containers run as an unprivileged user, with all Linux capabilities dropped (option A).
- ShinyProxy needs `/var/run/docker.sock`, which is root-equivalent on the host. Use a VM dedicated to this app.


## AI action plan (optional)

The app always computes a **team scorecard** (tab *Results > Team scorecard*) and can write a **rule-based action plan** without any AI. With an AI connection, *Results > Action plan (AI)* writes a team-by-team plan (issues with evidence, actions with owner and timing, interviewers to observe, modules to retrain) in English, French, Spanish, Portuguese or Arabic, and answers questions about the results.

**Choosing the model in the app**
- Pick a **Connection** (Claude, local Ollama, any OpenAI-compatible server), then a **Model**.
- **List models** reads the models available on that connection. For Ollama these are the installed models, largest first.
- You can also type a model name. **Test** checks that the model answers.
- If allowed, users can add **their own connection** (endpoint and/or key). It is kept in that session's memory only.

**Server configuration** (environment variables in the compose file; keys never reach the browser). Define any combination:

| Variable | Use |
|---|---|
| `ANTHROPIC_API_KEY` | Enables Claude (default model `claude-sonnet-5-5`, change with `IPM_AI_ANTHROPIC_MODEL`) |
| `IPM_AI_OLLAMA_URL` | Enables local models through Ollama, e.g. `http://192.168.1.10:11434` (optional `IPM_AI_OLLAMA_MODEL`) |
| `IPM_AI_OPENAI_URL`, `IPM_AI_OPENAI_KEY` | Enables any OpenAI-compatible server (vLLM, LM Studio, llama.cpp, OpenAI), e.g. `http://host:8000/v1` |
| `IPM_AI_PROVIDER`, `IPM_AI_MODEL` | Connection and model selected by default |
| `IPM_AI_ALLOW_CUSTOM` | `true` (default): users may add their own connection. Set `false` on a server open to people you don't trust, because a custom endpoint makes the server call that URL. |
| `IPM_AI_TIMEOUT` | Seconds; default 300, or 900 for Ollama |
| `IPM_AI_THINK` | `false` (default): turns off the "thinking" of reasoning models on Ollama (gemma4, qwen3, deepseek-r1). Otherwise they can use the whole output budget before answering. |

**What is sent to the model:** aggregated statistics only. That means the team and interviewer scorecard, flag rates per module and question, one-line readings of flagged questions, and interviewer codes. For a question naming a team, that team's flag tables (answer counts per interviewer) are added. No record-level data, household identifiers or names are sent. *Preview data sent to the AI* shows the exact payload: about 10-20k tokens per plan.

**Checks:** every plan is checked against the scorecard before display:
- teams that do not exist are removed;
- interviewer codes not in the team are marked;
- numbers that do not appear in the evidence are listed as "check before use".

**Local models**
- With Ollama, the app uses the native API. It sets the context window to fit the evidence (`num_ctx`, typically 32k), so nothing is truncated, and it passes the JSON schema so the output is always valid JSON.
- Plan quality depends on the model: use 14B+ parameters (e.g. `qwen2.5:14b`, `mistral-small:24b`). That needs about 12 GB+ of GPU memory for reasonable speed; on CPU, expect several minutes per plan.
- On a 4 GB GPU, use Claude or the rule-based plan.

**Data governance:** even aggregated, these are results of a national survey. Check that sending them to an external API is allowed by the NSO agreement and UNICEF rules, or use a local model.

## 6. Updating the app

New versions are published as GitHub releases (see NEWS.md for the changes):

```bash
sudo bash ipm-update.sh            # rebuilds ipm-app:latest from the latest release
docker compose up -d               # option A; on OMV press Up on the ipm file; option B uses the new image automatically
```

To go back to an earlier version, run `sudo bash ipm-update.sh v0.4.3` and restart the container the same way.

## 7. Troubleshooting

- `docker compose logs -f ipm` (or `shinyproxy`) shows errors.
- **Upload fails for big files:** raise `max_size` in the Caddyfile and `shiny.maxRequestSize` in `app_core.R` (400 MB by default).
- **Report building is cut off:** Caddy has no proxy timeout by default; check pfSense or any other proxy in between.
- **Out of memory on large national datasets:** raise `mem_limit` (option A) or `container-memory-limit` (option B) to 4g.

## OpenMediaVault (Compose plugin)

1. **Build the image** over SSH, as in step 2: `sudo bash ipm-update.sh`.
2. **Add the app in OMV.** Go to Services > Compose > Files > Create. Name it `ipm`, paste the content of `omv/ipm-compose.yml`, uncomment the AI lines you need, then Save and press **Up**.
3. **Open it** at `http://<omv-ip>:3838`. To reach it from outside, use the VPN, or a reverse proxy (Nginx Proxy Manager...) with **Websockets support on**, HTTPS, an access list, and `client_max_body_size 500M;`. Never forward port 3838 directly.
4. **Update:** run `sudo bash ipm-update.sh`, then press **Up** on the `ipm` file.

Keep the app in its own compose file and manage it from the command line or the *Files* page. Do not change OMV's Compose *Settings* page: applying it rewrites `/etc/docker/daemon.json` and restarts Docker.

### Data protection on an OMV box

- Uploaded files, results and reports live only in RAM inside the container (`tmpfs`). They never reach your data disks, the shared folders or Docker volumes.
- They are deleted when the browser tab is closed, after 30 minutes idle, or with "Delete my data now".
- If OMV has **swap** enabled (check with `swapon --show`), RAM pages can be written to the swap disk under memory pressure. To avoid this, either:
  - disable swap (`swapoff -a` and comment out the swap line in `/etc/fstab`) if the box has enough RAM for the media stack plus about 3 GB; or
  - keep `mem_limit` low enough that the host never needs to swap.
- If OMV runs as a Proxmox VM, do not back it up in "suspend" mode or with RAM included.
- `docker logs ipm` contains only start/stop and step messages, never data.

### Troubleshooting

| Problem | Fix |
|---|---|
| Page loads but stays grey / disconnects | The reverse proxy is not passing websockets. Enable "Websockets support". |
| `port is already allocated` | Change `"3838:3838"` to e.g. `"3939:3838"` in `omv/ipm-compose.yml`. |
| Upload of big files fails behind a proxy | Raise the proxy's body size limit. In Nginx Proxy Manager > Advanced, add `client_max_body_size 500M;`. |
| Container stops on big datasets | Raise `mem_limit` to `4g`. Check `docker stats ipm`. |
| See errors | `docker logs -f ipm` |
