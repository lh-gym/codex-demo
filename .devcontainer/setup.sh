#!/usr/bin/env bash
set -euo pipefail

# 0) 进入项目根目录（你原来就有）
cd /workspace/codex-demo || exit 1

# 1) Git 初始化：remote、fetch、切换/创建 han/validation（你原来就有）
git remote set-url origin https://github.com/lh-gym/codex-demo.git
git fetch origin
if git rev-parse --verify han/validation >/dev/null 2>&1; then
  git checkout han/validation
else
  git checkout -b han/validation
  git push -u origin han/validation
fi
echo "[devcontainer] Git branch setup complete. You're now on han/validation!"

# 2) 安装系统依赖：pandoc（rmarkdown 渲染 HTML 必需）
echo "[devcontainer] Installing pandoc..."
apt-get update -y
DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends pandoc

# 3) 安装 R 包（若缺失才装）：rmarkdown、knitr、VGAM
echo "[devcontainer] Installing R packages (rmarkdown, knitr, VGAM) if missing..."
R -q -e "options(Ncpus=parallel::detectCores());
req <- c('rmarkdown','knitr','VGAM');
miss <- setdiff(req, rownames(installed.packages()));
if (length(miss)) install.packages(miss, repos='https://cloud.r-project.org')"

echo "[devcontainer] Ready. You can render now with:"
echo "Rscript -e \"rmarkdown::render('PPO_playground.Rmd', output_format='html_document')\""
