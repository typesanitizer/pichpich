
struct X {
  /*
   * Block comment
   * NOTE(def: blah)
   */
  int x() {
    // Braces are interpreted loosely but that's OK
    fmt::format("{  }", 0);
  }
  /* Block comment with braces {} */
};

/* Funky block comment /* */
