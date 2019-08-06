export class Regex {

  public static readonly patterns = {

    // all auth IDs use this pattern
    // Note: the restriction to <= 64 chars is implemented by another validator,
    // so that we get a nicer error message. Here it's enough to restrict the
    // char set.
    // Interestingly, the regex validator doesn't seem to fire for empty strings.
    ID: '[0-9a-z-_]+',

    // NB: neither \S nor ^\s work inside the brackets in this regex language.
    NON_BLANK: '.*[^ ].*'
  };

}
