Various utilities to work with Wikipedia

* `dump-category.lisp` Dumps category contents using MediaWiki API

* `missing-topics.lisp` Given a list of categories query all articles
  in them and compile a list of missing articles (most popular red
  links). This is somewhat similar to the missing topics tool, but
  works by querying the Wikipedia server API and uses a dumped list
  of all article titles

* `page-views.lisp` Uses the [Wikimedia
  PageviewsAPI](https://wikitech.wikimedia.org/wiki/Analytics/PageviewAPI)
  to obtain information on the number of visitors to a page

* `project-stats.lisp` Given a Wiki project name generates Top-100
  table and a summary. It is basically a combination of dump category
  contents and the page views functions.