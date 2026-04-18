# md4s

[![build](https://img.shields.io/github/actions/workflow/status/wlad031/md4s/scala.yml?label=CI&logo=GitHub&style=flat-square)](https://github.com/wlad031/md4s/actions)
[![codecov](https://img.shields.io/codecov/c/github/wlad031/md4s?label=cov&logo=Codecov&style=flat-square)](https://codecov.io/gh/wlad031/md4s)

Markdown parsers/formatters for Scala, focused on Logseq and Obsidian flavors.

## Modules

- `dev.vgerasimov.md4s.logseq`: Logseq Markdown parser, formatter, updater.
- `dev.vgerasimov.md4s.obsidian`: Obsidian Markdown parser and formatter.
- `dev.vgerasimov.md4s.interop`: Java/Clojure-friendly API over both parsers.

## Scala usage

```scala
import dev.vgerasimov.md4s.obsidian
import dev.vgerasimov.slowparse.POut

val parser = new obsidian.Parser()
parser.document("## heading") match
  case POut.Success(doc, _, _, _) =>
    val text = obsidian.Formatter().format(doc)
  case POut.Failure(message, _) =>
    println(message)
```

## Java usage

```java
import dev.vgerasimov.md4s.interop.Parsers;

var result = Parsers.parseObsidian("## heading");
if (result.isSuccess()) {
  var doc = result.getDocument().orElseThrow();
}

var data = Parsers.parseObsidianAsData("# title");
```

## Clojure usage

```clojure
(def result (dev.vgerasimov.md4s.interop.Parsers/parseObsidianAsData "# title"))
(when (.isSuccess result)
  (def doc-map (.orElseThrow (.getDocument result))))
```

`parse*AsData` returns plain Java `Map`/`List` tree with `_type` field for each product node.
