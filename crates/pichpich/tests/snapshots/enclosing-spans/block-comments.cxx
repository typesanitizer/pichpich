// Currently block comments don't do paragraph detection,
// so we only have 2 test cases.

//-----------------------------------------------------------------------------
// ( 1) top-level

/*
  Some stuff here
  Other stuff.

  New paragraph.
  NOTE(def: bc-1-1): doo doo doo
  Other stuff

  Some text
  See also NOTE(ref: bc-1-1)

  NOTE(def: bc-1-2): la la la

*/

/*
 * New paragraph.
 * NOTE(def: bc-1-3): doo doo doo
 * Other stuff
 *
 * Some text
 * See also NOTE(ref: bc-1-3)
 *
 * NOTE(def: bc-1-4): la la la
 *
 */

/* NOTE(def: bc-1-5): Cookies */
/* See NOTE(def: bc-1-5) */

//-----------------------------------------------------------------------------
// ( 2) in braces

{
  {
    { {} }
    /*
      Some stuff here
      Other stuff.

      New paragraph.
      NOTE(def: bc-2-1): doo doo doo
      Other stuff

      Some text
      See also NOTE(ref: bc-2-1)

      NOTE(def: bc-2-2): la la la

    */

    /*
     * New paragraph.
     * NOTE(def: bc-2-3): doo doo doo
     * Other stuff
     *
     * Some text
     * See also NOTE(ref: bc-2-3)
     *
     * NOTE(def: bc-2-4): la la la
     *
     */

    /* NOTE(def: bc-2-5): Cookies */
    /* See NOTE(def: bc-2-5) */
    { {} }
  }
}