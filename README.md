# A Tool to Format the Link-title-content for a Website

This is a command line interface (CLI) tool to concat the **Title** and **Link** or other contents into markdown format with REPL process.

It includes four main mode:
- item
  - Input `contents` and  output `- contents`, the non-number list expression of markdown.
- links
  - Input `link` and output `- <link>`, i.e., the link expression of markdown.
- pair
  - Input `title` and `link`, then output `- title: <link>`.
- match
  - Input `title` and `contents`, then output `- title: contents`.
