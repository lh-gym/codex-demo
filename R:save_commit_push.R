save_commit_push <- function(msg = NULL) {
  # 1) 保存所有编辑器文件
  if (rstudioapi::isAvailable()) rstudioapi::documentSaveAll()
  
  # 2) 生成默认提交说明（可改成你喜欢的格式）
  if (is.null(msg) || msg == "") {
    msg <- sprintf("auto: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  }
  
  # 3) 提交并推送（无改动时不报错）
  try({
    gert::git_add(".")
    if (length(gert::git_status()$file) > 0) {
      gert::git_commit(message = msg)
      gert::git_push()
    }
  }, silent = TRUE)
# 4) 提示完成
  message("All changes saved, committed, and pushed.")
}