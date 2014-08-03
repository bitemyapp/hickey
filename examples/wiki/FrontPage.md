Welcome to the amazing and exciting **Hickey**!

Hickey is a git-backed wiki in Haskell using [Pandoc-flavoured Markdown](http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html) for formatting.

You can create a new page by simply editing one which doesn't already exist, so dive right in!

## Concepts:

 - Page names are alphanumeric strings.

    Eventually WikiWords will be linked automatically.

 - A page can have files "attached" to it.

    You can still reference any file from any page, but for simplicity every file has a page which "owns" it.

 - Everything is stored in revision control (git, specifically).
    - We get really nice merging of changes for free!

        If you and someone else edit a page at the same time, the second one to save their changes will be presented with the attempted merge (most likely a merge conflict) to resolve.

    - A wiki can be moved around, backed up, and deployed in multiple simultaneous places just using your standard VCS commands.

 - Plugins (will) exist!

    Re-using Pandoc's fenced code block syntax, you'll be able to send off blocks of text to a plugin to produce markdown from. Plugins will just be shellscripts (or any other executable) living in the wiki repository under a special directory.

### Semantic HTML!

The wiki is all HTML5, and Pandoc generates HTML with nice use of `<section>` blocks, making the structure of the output very amenable to machine processing. The non-pandoc bits have also been marked up appropriately to avoid ruining this.

### You may have noticed…

…the table of contents over to the side there. Yes, just over to your right. For pages which contain headings, a table of contents is rendered to ease navigation. Currently this is done for all pages with headings, but I might add an option to restrict to when there are nested headings or something.

## Source

Hickey is licensed under the WTFPL (Do What The Fuck You Want To Public License),

> |         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
> |                     Version 2, December 2004 
> | 
> |  Copyright (C) 2004 Sam Hocevar <sam@hocevar.net> 
> | 
> |  Everyone is permitted to copy and distribute verbatim or modified 
> |  copies of this license document, and changing it is allowed as long 
> |  as the name is changed. 
> | 
> |             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
> |    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
> | 
> |   0. You just DO WHAT THE FUCK YOU WANT TO.

The source code is available [on GitHub](https://github.com/barrucadu/hickey).