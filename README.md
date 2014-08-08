Hickey: A (Haskell) Revisioning Wiki
====================================

Hickey is a git-backed wiki written in Haskell using Pandoc-flavoured
Markdown for pages, and supporting rendering plugins through external
executables. Every single change, be it editing a page or uploading a
file, is recorded as a separate commit - giving a clean and useful
history, friendly to use with external tools.

Features:

 - Automatic linking of WikiWords.

 - `[EmptyLinks]()` link to the page of the same name.

 - Highlight broken wiki links.

 - Inter-wiki links.

 - Call external plugins by using fenced code blocks.

 - Pages can have file attachments.

In addition, *everything* is done through git, the only configuration
external to that is the repo path: you can clone an entire wiki and
redeploy elsewhere with a regular `git clone` and starting a new
instance of Hickey.

Planned:

 - Automatically-updated category listings.
 - Syntactic niceties for linking to specific revisions of pages.

Plugins
-------

Pandoc introduces a fenced code block syntax to Markdown, where you
can specify the language (or other properties) of the code contained
within, for example:

    ~~~~{.haskell}
    main :: IO ()
    main = putStrLn "Hello, world!"
    ~~~~

This will render with Haskell-specific syntax highlighting. However,
by replacing "haskell" with the name of a plugin ("dot" is provided by
default), then that plugin is executed.

Specifically, if a block has a single attribute, and the value of that
attribute corresponds to the filename of something in the plugins/
directory of the repository, then that file is executed with the
contents of the block as stdin, and the plugin emits Pandoc-flavoured
Markdown on stdout.

This lets us write this:

    ~~~~{.dot}
    graph graphname {
        a [label="Foo"];
        b [shape=box];
        a -- b -- c [color=blue];
        b -- d [style=dotted];
    }
    ~~~~

…and get an image embedded into the page.

Plugin expansion *is* recursive, so plugins can generate code to be
expanded by other plugins, but there is a depth limit of 100 to
prevent infinite loops.

Inter-wiki Links
----------------

Wikis are all about linking things together, but linking to other
wikis typically requires you to use a full URL to the article you want
to link to. Some wikis offer a shorthand, called inter-wiki linking,
where you associate with an external wiki a prefix, and then you can
link to articles on it in exactly the same way as you link to internal
articles, but with the addition of adding on the defined prefix.

Hickey stores inter-wiki link configuration in the interwiki.conf file
in the root of the repository. Wikipedia is included as an
example. When rendering, the `{}` is replaced with the link target
(minus the prefix), allowing you to define links like `wikipedia:Foo`,
or `[wikipedia:List of Lists]()`, which will work as you expect.

Examples
--------

The examples/ directory includes a sample configuration file, which
simply specifies the path to the repository (which can be absolute or
relative to Hickey's working directory) and a sample wiki structure.

You can get started by just copying the contents of the sample wiki to
a git repository and committing them.

Notes
-----

Hickey uses git for everything, including static files. If you're
having difficulty accessing something, make sure you committed it, and
didn't simply place it in the directory structure.

Haddock documentation generated from the code, in a reasonably
up-to-date format, can be accessed [here][documentation].

[documentation]: http://runciman.hacksoc.org/~barrucadu/hickey/
