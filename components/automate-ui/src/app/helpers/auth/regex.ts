export class Regex {

  public static readonly patterns = {
    // NB: neither \S nor ^\s work inside the brackets in this regex language.
    NON_BLANK: '.*[^ ].*'
  };

}
