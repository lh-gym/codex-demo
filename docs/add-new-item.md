# 添加新内容

1. 将新的独立资料放入 `items/topic-name/`；需要保持简短、稳定 URL 的页面也可以放在仓库根目录。
2. 公共图片、音频和下载文件放入 `assets/`，或与其对应页面放在同一目录。
3. 文件夹使用小写英文；单词间用连字符，例如 `items/company-name/interview.html`。
4. 在 `content/catalog.js` 的 `window.SITE_LIBRARY_ITEMS` 中加入一个对象。
5. 确认 `id` 唯一、`href` 指向存在的文件，状态为 `ready`、`draft` 或 `archive`。
6. 本地打开首页，验证搜索、筛选、页面链接及媒体播放。

## 公开仓库安全提醒

GitHub Pages 内容可能被公开访问。不要上传身份证件、密码、令牌、私人简历、受保密义务约束的客户资料，或未经授权公开的录音与个人信息。
