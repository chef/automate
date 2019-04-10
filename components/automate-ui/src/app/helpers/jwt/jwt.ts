import * as jwtDecode from 'jwt-decode';

// This describes an id_token as used in OpenID Connect (OIDC) -- see here for
// its definition in the spec:
//   http://openid.net/specs/openid-connect-core-1_0.html#IDToken

// Note: we don't use all of these -- we only really care for
// - name
// - groups
// - email
export interface IDToken {
  sub: string;
  iss: string;
  aud: string;
  exp: number;
  iat: number;
  at_hash: string;
  email: string;
  email_verified: boolean;
  name: string;
  groups: Array<string>;
}

export interface Entitlement {
  name: string;
  measure: string;
  limit: number;
  start: {
    seconds: number;
  };
  end: {
    seconds: number;
  };
}

export interface LicenseToken {
  entitlements: Array<Entitlement>;
}

export class Jwt {

  static parseIDToken(token: string): IDToken | null {
    let tok: IDToken;
    try {
      tok = jwtDecode(token);
    } catch (e) {
      return null;
    }
    return tok;
  }

  static parseLicenseToken(token: string): LicenseToken | null {
    let tok: LicenseToken;
    try {
      tok = jwtDecode(token);
    } catch (e) {
      return null;
    }
    return tok;
  }
}
