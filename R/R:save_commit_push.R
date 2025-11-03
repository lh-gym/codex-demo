save_commit_push <- function(msg = NULL) {
  # 1️⃣ 保存当前文件（如果可用）
  if (rstudioapi::isAvailable()) rstudioapi::documentSaveAll()
  
  # 2️⃣ 生成默认提交信息
  if (is.null(msg) || msg == "") {
    ctx <- rstudioapi::getSourceEditorContext()
    file <- if (!is.null(ctx$path)) basename(ctx$path) else "unknown_file"
    msg <- sprintf("auto: save %s @ %s", file, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
  }
  
  # 3️⃣ 执行 git 操作（使用 gert 包最稳）
  try({
    gert::git_add(".")
    if (length(gert::git_status()$file) > 0) {
      gert::git_commit(message = msg)
      gert::git_push()
      message("✅ All changes saved, committed, and pushed.")
    } else {
      message("⚠️ No changes to commit.")
    }
  }, silent = TRUE)
}
