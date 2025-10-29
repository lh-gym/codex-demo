#!/bin/bash
# 🚀 自动连接 GitHub 仓库并切换分支 (for Codex / Codespaces)

# 1️⃣ 进入项目根目录
cd /workspace/codex-demo || exit 1

# 2️⃣ 设置远程仓库地址
git remote set-url origin https://github.com/lh-gym/codex-demo.git

# 3️⃣ 拉取远程分支信息
git fetch origin

# 4️⃣ 尝试切换到 han/validation 分支，如果不存在则新建
if git rev-parse --verify han/validation >/dev/null 2>&1; then
  git checkout han/validation
else
  git checkout -b han/validation
  git push -u origin han/validation
fi

# 5️⃣ 输出提示
echo "✅ Git branch setup complete. You're now on han/validation!"
