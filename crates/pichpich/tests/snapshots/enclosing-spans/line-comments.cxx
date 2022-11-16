
// Matrix of test cases for line comments:
// - [top-level, in braces]
// - [paragraph start, NOT paragraph start]
// - [paragraph end, NOT paragraph end]
// - [line start, NOT line start]
//
// So it is a total of 16 cases.

//-----------------------------------------------------------------------------
// ( 1) top-level, paragraph start, paragraph end, line start

// NOTE(def: lc-1-1) aa

//
// NOTE(def: lc-1-2) bb

// NOTE(def: lc-1-3)
//

//
// NOTE(def: lc-1-4)
//

//-----------------------------------------------------------------------------
// ( 2) top-level, paragraph start, paragraph end, NOT line start

// This is NOTE(def: lc-2-1) aa

//
// This is NOTE(def: lc-2-2) bb

// This is NOTE(def: lc-2-3)
//

//
// This is NOTE(def: lc-2-4)
//

//-----------------------------------------------------------------------------
// ( 3) top-level, paragraph start, NOT paragraph end, line start

// NOTE(def: lc-3-1) aa
// Blah blah

//
// NOTE(def: lc-3-2) bb
// Blah blah

// NOTE(def: lc-3-3)
// Blah blah
//

//
// NOTE(def: lc-3-4)
// Blah blah
//

//-----------------------------------------------------------------------------
// ( 4) top-level, paragraph start, NOT paragraph end, NOT line start

// This is NOTE(def: lc-4-1) aa
// Blah blah

//
// This is NOTE(def: lc-4-2) bb
// Blah blah

// This is NOTE(def: lc-4-3)
// Blah blah
//

//
// This is NOTE(def: lc-4-4)
// Blah blah
//

//-----------------------------------------------------------------------------
// ( 5) top-level, NOT paragraph start, paragraph end, line start

// Blah blah
// NOTE(def: lc-5-1) aa

//
// Blah blah
// NOTE(def: lc-5-2) bb

// Blah blah
// NOTE(def: lc-5-3)
//

//
// Blah blah
// NOTE(def: lc-5-4)
//

//-----------------------------------------------------------------------------
// ( 6) top-level, NOT paragraph start, paragraph end, NOT line start

// Blah blah
// This is NOTE(def: lc-6-1) aa

//
// Blah blah
// This is NOTE(def: lc-6-2) bb

// Blah blah
// This is NOTE(def: lc-6-3)
//

//
// Blah blah
// This is NOTE(def: lc-6-4)
//

//-----------------------------------------------------------------------------
// ( 7) top-level, NOT paragraph start, NOT paragraph end, line start

// Blah blah
// NOTE(def: lc-7-1) aa
// Even more blah blah

//
// Blah blah
// NOTE(def: lc-7-2) bb
// Even more blah blah

// Blah blah
// NOTE(def: lc-7-3)
// Even more blah blah
//

//
// Blah blah
// NOTE(def: lc-7-4)
// Even more blah blah
//

//-----------------------------------------------------------------------------
// ( 8) top-level, NOT paragraph start, NOT paragraph end, NOT line start

// Blah blah
// This is NOTE(def: lc-8-1) aa
// Even more blah blah

//
// Blah blah
// This is NOTE(def: lc-8-2) bb
// Even more blah blah

// Blah blah
// This is NOTE(def: lc-8-3)
// Even more blah blah
//

//
// Blah blah
// This is NOTE(def: lc-8-4)
// Even more blah blah
//

//-----------------------------------------------------------------------------
// ( 9) in group, paragraph start, paragraph end, line start

{
  {
    // NOTE(def: lc-9-1) aa
    { }
    //
    // NOTE(def: lc-9-2) bb

    // NOTE(def: lc-9-3)
    //
    { {} }
    //
    // NOTE(def: lc-9-4)
    //
  }
}

//-----------------------------------------------------------------------------
// (10) in group, paragraph start, paragraph end, NOT line start

{
  {
    // This is NOTE(def: lc-10-1) aa
    { }
    //
    // This is NOTE(def: lc-10-2) bb

    // This is NOTE(def: lc-10-3)
    //
    { {} }
    //
    // This is NOTE(def: lc-10-4)
    //
  }
}

//-----------------------------------------------------------------------------
// (11) in group, paragraph start, NOT paragraph end, line start

{
  {
    // NOTE(def: lc-11-1) aa
    // Blah blah
    { }
    //
    // NOTE(def: lc-11-2) bb
    // Blah blah

    // NOTE(def: lc-11-3)
    // Blah blah
    //
    { {} }
    //
    // NOTE(def: lc-11-4)
    // Blah blah
    //
  }
}

//-----------------------------------------------------------------------------
// (12) in group, paragraph start, NOT paragraph end, NOT line start

{
  {
    // This is NOTE(def: lc-12-1) aa
    // Blah blah
    { }
    //
    // This is NOTE(def: lc-12-2) bb
    // Blah blah

    // This is NOTE(def: lc-12-3)
    // Blah blah
    //
    { {} }
    //
    // This is NOTE(def: lc-12-4)
    // Blah blah
    //
  }
}


//-----------------------------------------------------------------------------
// (13) in group, NOT paragraph start, paragraph end, line start

{
  {
    // Blah blah
    // NOTE(def: lc-13-1) aa
    { }
    //
    // Blah blah
    // NOTE(def: lc-13-2) bb

    // Blah blah
    // NOTE(def: lc-13-3)
    //
    { {} }
    //
    // Blah blah
    // NOTE(def: lc-13-4)
    //
  }
}

//-----------------------------------------------------------------------------
// (14) in group, NOT paragraph start, paragraph end, NOT line start

{
  {
    // Blah blah
    // This is NOTE(def: lc-14-1) aa
    { }
    //
    // Blah blah
    // This is NOTE(def: lc-14-2) bb

    // Blah blah
    // This is NOTE(def: lc-14-3)
    //
    { {} }
    //
    // Blah blah
    // This is NOTE(def: lc-14-4)
    //
  }
}

//-----------------------------------------------------------------------------
// (15) in group, NOT paragraph start, NOT paragraph end, line start

{
  {
    // Blah blah
    // NOTE(def: lc-15-1) aa
    // Even more blah blah
    { }
    //
    // Blah blah
    // NOTE(def: lc-15-2) bb
    // Even more blah blah

    // Blah blah
    // NOTE(def: lc-15-3)
    // Even more blah blah
    //
    { {} }
    //
    // Blah blah
    // NOTE(def: lc-15-4)
    // Even more blah blah
    //
  }
}

//-----------------------------------------------------------------------------
// (16) in group, NOT paragraph start, NOT paragraph end, NOT line start

{
  {
    // Blah blah
    // This is NOTE(def: lc-8-1) aa
    // Even more blah blah
    { }
    //
    // Blah blah
    // This is NOTE(def: lc-8-2) bb
    // Even more blah blah

    // Blah blah
    // This is NOTE(def: lc-8-3)
    // Even more blah blah
    //
    { {} }
    //
    // Blah blah
    // This is NOTE(def: lc-8-4)
    // Even more blah blah
    //
  }
}
