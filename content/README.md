# Catalog schema

`catalog.js` is the single source of truth for cards on the homepage. It defines `window.SITE_LIBRARY_ITEMS`.

Each item uses:

- `id`: stable lowercase hyphen-case identifier
- `title`: human-readable title
- `href`: path relative to the repository root
- `kind`: `page`, `file`, or `notebook`
- `area`: short topic such as `interview`
- `status`: `ready`, `draft`, or `archive`
- `updated`: `YYYY-MM-DD`
- `owner`: short owner label
- `featured`: boolean
- `summary`: one-sentence description
- `tags`: searchable string array

Keep existing `href` values stable after publication so public URLs do not break.
