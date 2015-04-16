`cl-mediawiki-v` is a client-side API to access Wiki servers built on MediaWiki engine. Basically it lets to develop bots for Wikipedia/Wikimedia projects.

This is a fork of the [cl-mediawiki](https://github.com/AccelerationNet/cl-mediawiki) library developed by AccelerationNet. This fork, unlike original, uses Json as the data exchange format with the server.

The `util` directory has some basic tools developed with this library:

* `catstree.lisp` walks the directories tree starting from the given
  directory and produces a list of all directories it walked through.

* `missing-topics.lisp` walks a list of directories, compiles a list
  of articles in these directories and produces a list of missing
  (red) links. Then it sorts them based on the popularity and prints a
  wiki table. Something like a missing topics tool.

## Usage

This library is not yet present in the Quicklisp repository. In order to use it follow the relevant instructions (*Can I load a local project that isn't part of Quicklisp?*) in the [Quicklisp FAQ](http://www.quicklisp.org/beta/faq.html).

Functions are exported into the `cl-mediawiki` and `wiki` package names.

Use `with-mediawiki` macro to initialize the wiki client API instance:

    (wiki:with-mediawiki ("http://uk.wikipedia.org/w")
      ;; put your code here
    )

Currently we provide following functions:

*Query*

* `login`
* `siteinfo`
* `userinfo`
* `get-page-content`
* `get-action-tokens`
* `recent-changes`
* `user-contribs`
* `list-category-members`
* `get-page-to-edit`
* `get-page-links`

*Edit*
* `set-page-content`
* `append-text-to-page`
* `prepend-text-to-page`
* `add-new-page-section`
* `create-page`
* `get-page-info`
* `pages-that-embed`

*Util*

* `user-anon-p`
* `has-messages-p`

Some of them are tested and used more often and are considered operational. See the supplied utils for usage examples.