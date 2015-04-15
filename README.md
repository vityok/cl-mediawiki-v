`cl-mediawiki-v` is a client-side API to access Wiki servers built on MediaWiki engine. Basically it lets to develop bots for Wikipedia/Wikimedia projects.

This is a fork of the [cl-mediawiki](https://github.com/AccelerationNet/cl-mediawiki) library developed by AccelerationNet. This fork, unlike original, uses Json as the data exchange format with the server.

The `util` directory has some basic tools developed with this library:

* `catstree.lisp` walks the directories tree starting from the given
  directory and produces a list of all directories it walked through.

* `missing-topics.lisp` walks a list of directories, compiles a list
  of articles in these directories and produces a list of missing
  (red) links. Then it sorts them based on the popularity and prints a
  wiki table. Something like a missing topics tool.
