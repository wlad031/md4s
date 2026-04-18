package dev.vgerasimov.md4s.interop;

import java.util.Map;

public final class Parsers {

  private static final ParsersInterop DELEGATE = new ParsersInterop();

  private Parsers() {
  }

  public static ParsersInterop delegate() {
    return DELEGATE;
  }

  public static ParseResult<dev.vgerasimov.md4s.logseq.models.Document> parseLogseq(String input) {
    return DELEGATE.parseLogseq(input);
  }

  public static ParseResult<dev.vgerasimov.md4s.obsidian.models.Document> parseObsidian(String input) {
    return DELEGATE.parseObsidian(input);
  }

  public static dev.vgerasimov.md4s.logseq.models.Document parseLogseqOrThrow(String input) {
    return DELEGATE.parseLogseqOrThrow(input);
  }

  public static dev.vgerasimov.md4s.obsidian.models.Document parseObsidianOrThrow(String input) {
    return DELEGATE.parseObsidianOrThrow(input);
  }

  public static ParseResult<Map<String, Object>> parseLogseqAsData(String input) {
    @SuppressWarnings("unchecked")
    ParseResult<Map<String, Object>> result = (ParseResult<Map<String, Object>>) (ParseResult<?>) DELEGATE.parseLogseqAsData(input);
    return result;
  }

  public static ParseResult<Map<String, Object>> parseObsidianAsData(String input) {
    @SuppressWarnings("unchecked")
    ParseResult<Map<String, Object>> result = (ParseResult<Map<String, Object>>) (ParseResult<?>) DELEGATE.parseObsidianAsData(input);
    return result;
  }

  public static String formatLogseq(dev.vgerasimov.md4s.logseq.models.Document document) {
    return DELEGATE.formatLogseq(document);
  }

  public static String formatObsidian(dev.vgerasimov.md4s.obsidian.models.Document document) {
    return DELEGATE.formatObsidian(document);
  }
}
