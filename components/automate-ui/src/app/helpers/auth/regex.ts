export class Regex {

  public static readonly patterns = {

    // all auth IDs use this pattern
    // Note: the restriction to <= 64 chars is implemented by another validator,
    // so that we get a nicer error message. Here it's enough to restrict the
    // char set.
    // Interestingly, the regex validator doesn't seem to fire for empty strings.
    ID: '[0-9a-z-_]+',

    // NB: neither \S nor ^\s work inside the brackets in this regex language.
    NON_BLANK: '.*[^ ].*',

    // Only allows wildcard alone or words and numbers, but not combined
    // Legal Values: *, chef, _state, etc.
    // Illegal Values: abc*, *chef, c*h*e*f, **

    // Allows no special characters except hyphen and underscore.
    NO_MIXED_WILDCARD_ALLOW_HYPHEN: '^(\\*|[-\\w]+)$',

    // Allows all special characters except colon :
    NO_MIXED_WILDCARD_ALLOW_SPECIAL: '^(\\*|[^:*]+)$',

    // Allows valid FQDN only
    // Top level domain is limited to a maximum number of 25 characters
    VALID_FQDN: /^(http:\/\/www\.|https:\/\/www\.|http:\/\/|https:\/\/)?[a-zA-Z0-9_]+([\-\.]{1}[a-zA-Z0-9]+)*\.[a-zA-Z]{2,25}(:[0-9]{1,5})?(\/.*)?$/,
    // Allows valid IP Address only (ipv4)
    VALID_IP_ADDRESS: '^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$',

    // Allow valid versions only Eg: 1.2.3
    VALID_VERSION: /^\d{1,9}\.\d{1,9}\.\d{1,9}$/
  };

}
