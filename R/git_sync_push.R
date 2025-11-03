# ---- git_sync_push.R ----
git_sync_push <- function(msg = NULL) {
  # 依赖检查
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !requireNamespace("gert", quietly = TRUE)) {
    stop("请先安装 rstudioapi 和 gert 包: install.packages(c('rstudioapi','gert'))")
  }

  # 1️⃣ 保存当前文件
  if (rstudioapi::isAvailable()) {
    rstudioapi::documentSaveAll()
    message("💾 所有文件已保存。")
  }

  # 2️⃣ 拉取远程更新（合并方式）
  message("⬇️ 正在从远程拉取最新更改...")
  tryCatch({
    gert::git_pull(rebase = FALSE)
    message("✅ 已成功拉取最新更改。")
  }, error = function(e) {
    message("⚠️ 拉取失败：", e$message)
  })

  # 3️⃣ 自动生成 commit 信息
  if (is.null(msg) || msg == "") {
    msg <- paste0("auto-sync @ ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }

  # 4️⃣ 提交 & 推送
  tryCatch({
    gert::git_add(".")
    status <- gert::git_status()
    if (nrow(status) > 0) {
      gert::git_commit(message = msg)
      gert::git_push()
      message("🚀 已成功提交并推送到远程。")
    } else {
      message("⚠️ 无文件变化，无需提交。")
    }
  }, error = function(e) {
    message("⚠️ 推送失败：", e$message)
  })
}
