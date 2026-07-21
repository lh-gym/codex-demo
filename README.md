# Interview Records

一个无需构建步骤的静态 GitHub Pages 资料库，包含面试录音、英文转写和中文翻译。

- [打开本地首页](./index.html)
- [添加新内容流程](./docs/add-new-item.md)

## 结构

- `index.html`：可搜索、筛选的资料库首页
- `content/catalog.js`：首页卡片的单一数据源
- `docs/`：维护说明
- `items/`：未来按主题添加的内容
- `assets/`：共享资源
- 根目录双语 HTML：已发布的稳定页面 URL

## 本地预览

```sh
python3 -m http.server 8000
```

然后打开 `http://127.0.0.1:8000/`。

> 隐私：GitHub Pages 内容可能公开访问。发布前必须确认录音和转写获准公开。
