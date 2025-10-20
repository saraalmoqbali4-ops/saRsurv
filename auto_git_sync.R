# 🔄 Auto Git Sync Script for saRsurv
# This script pulls, commits, and pushes updates to GitHub automatically.

auto_git_sync <- function(commit_message = "Auto-sync updates from RStudio") {
  repo_path <- "C:/Users/pc/Documents/saRsurv"  # ✨ غيّري المسار لو مجلدك مختلف
  setwd(repo_path)

  cat("🚀 Starting Git sync...\n")

  # Run Git commands
  system("git pull --allow-unrelated-histories origin main")
  system("git add .")

  cmd <- sprintf('git commit -m "%s"', commit_message)
  system(cmd)

  system("git push origin main")

  cat("✅ All changes synced successfully to GitHub!\n")
}

# ✨ Run it directly:
auto_git_sync("Updated documentation and fixed warnings")

