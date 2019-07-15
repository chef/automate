export class Regex {

  public static readonly patterns = {

    // all auth IDs use this pattern
    ID: '[0-9a-z-]+',

    // NB: neither \S nor ^\s work inside the brackets in this regex language.
    NON_BLANK: '.*[^ ].*'
  };

}
