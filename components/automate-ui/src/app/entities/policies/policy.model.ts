// Must be kept in sync with automate-gateway/api/iam/v2beta/common/policy.proto
export interface Policy {
  id: string;
  name: string;
  type: IAMType;
  members: string[];
  statements?: Statement[];
}

export interface Statement {
  effect: 'ALLOW' | 'DENY';
  role?: string;
  actions?: string[];
  resources?: string[];
}

export type IAMType = 'CHEF_MANAGED' | 'CUSTOM';
export type IAMMajorVersion = 'v1' | 'v2';
export type IAMMinorVersion = 'v0' | 'v1';

export interface Member {
  name: string;
  displayName: string;
  displayType: string;
  type: Type;
}

export function memberListToStringList(members: Member[]): string[] {
  return members.map(m => m.name);
}

// policyFromPayload does some cleanup steps on the API response (removing
// empty fields, like "" and [], from statements). Ideally, the API would
// return a nice response; but it's a bit simpler to just clean up here.
export function policyFromPayload(policy: Policy): Policy {
  if (policy.statements) {
    policy.statements.forEach(stmt =>
      Object.keys(stmt).
        filter(key => stmt[key] === '' || (Array.isArray(stmt[key]) && stmt[key].length === 0)).
        forEach(key => delete stmt[key]));
  }
  return policy;
}

export enum Type {
  AllMembers = 1,
  AllUsers,
  AllTeams,
  AllTokens,
  AllLocalUsers,
  LocalUser,
  AllLocalTeams,
  LocalTeam,
  AllLDAPUsers,
  LDAPUser,
  AllLDAPTeams,
  LDAPTeam,
  AllSAMLUsers,
  SAMLUser,
  AllSAMLTeams,
  SAMLTeam,
  ALlTokens,
  Token,
  Unknown
}

export function stringToMember(memberString: string): Member {
  const member = <Member>{};
  member.name = memberString;
  if (memberString.split('').pop() === ':') {
    member.type = Type.Unknown;
    return member;
  }
  const term = memberString.split(':');
  switch (term[0]) {
    case '*': {
      if (term.length !== 1) {
        member.type = Type.Unknown;
        break;
      }
      member.type = Type.AllMembers;
      member.displayType = member.displayName = 'All members';
      break;
    }
    case 'user': {
      if (term.length < 2 || term.length > 3) {
        member.type = Type.Unknown;
        break;
      }
      switch (term[1]) {
        case '*': {
          if (term.length !== 2) {
            member.type = Type.Unknown;
            break;
          }
          member.type = Type.AllUsers;
          member.displayType = member.displayName = 'All users';
          break;
        }
        case 'local': {
          if (term.length !== 3) {
            member.type = Type.Unknown;
            break;
          }
          switch (term[2]) {
            case '*': {
              member.type = Type.AllLocalUsers;
              member.displayType = member.displayName = 'All local users';
              break;
            }
            default: {
              member.type = Type.LocalUser;
              member.displayName = term[2];
              member.displayType = 'Local user';
              break;
            }
          }
          break;
        }
        case 'ldap': {
          if (term.length !== 3) {
            member.type = Type.Unknown;
            break;
          }
          switch (term[2]) {
            case '*': {
              member.type = Type.AllLDAPUsers;
              member.displayType = member.displayName = 'All LDAP users';
              break;
            }
            default: {
              member.type = Type.LDAPUser;
              member.displayName = term[2];
              member.displayType = 'LDAP user';
              break;
            }
          }
          break;
        }
        case 'saml': {
          if (term.length !== 3) {
            member.type = Type.Unknown;
            break;
          }
          switch (term[2]) {
            case '*': {
              member.type = Type.AllSAMLUsers;
              member.displayType = member.displayName = 'All SAML users';
              break;
            }
            default: {
              member.type = Type.SAMLUser;
              member.displayName = term[2];
              member.displayType = 'SAML user';
              break;
            }
          }
          break;
        }
        default: {
          member.type = Type.Unknown;
          break;
        }
      }
      break;
    }
    case 'team': {
      if (term.length < 2 || term.length > 3) {
        member.type = Type.Unknown;
        break;
      }
      switch (term[1]) {
        case '*': {
          if (term.length !== 2) {
            member.type = Type.Unknown;
            break;
          }
          member.type = Type.AllTeams;
          member.displayType = member.displayName = 'All teams';
          break;
        }
        case 'local': {
          if (term.length !== 3) {
            member.type = Type.Unknown;
            break;
          }
          switch (term[2]) {
            case '*': {
              member.type = Type.AllLocalTeams;
              member.displayType = member.displayName = 'All local teams';
              break;
            }
            default: {
              member.type = Type.LocalTeam;
              member.displayName = term[2];
              member.displayType = 'Local team';
              break;
            }
          }
          break;
        }
        case 'ldap': {
          if (term.length !== 3) {
            member.type = Type.Unknown;
            break;
          }
          switch (term[2]) {
            case '*': {
              member.type = Type.AllLDAPTeams;
              member.displayType = member.displayName = 'All LDAP teams';
              break;
            }
            default: {
              member.type = Type.LDAPTeam;
              member.displayName = term[2];
              member.displayType = 'LDAP team';
              break;
            }
          }
          break;
        }
        case 'saml': {
          if (term.length !== 3) {
            member.type = Type.Unknown;
            break;
          }
          switch (term[2]) {
            case '*': {
              member.type = Type.AllSAMLTeams;
              member.displayType = member.displayName = 'All SAML teams';
              break;
            }
            default: {
              member.type = Type.SAMLTeam;
              member.displayType = 'SAML team';
              member.displayName = term[2];
              break;
            }
          }
          break;
        }
        default: {
          member.type = Type.Unknown;
          break;
        }
      }
      break;
    }
    case 'token': {
      if (term.length !== 2) {
        member.type = Type.Unknown;
        break;
      }
      switch (term[1]) {
        case '*': {
          member.type = Type.AllTokens;
          member.displayType = member.displayName = 'All API tokens';
          break;
        }
        default: {
          member.type = Type.Token;
          member.displayName = term[1];
          member.displayType = 'API token';
          break;
        }
      }
      break;
    }
    default: {
      member.type = Type.Unknown;
      break;
    }
  }
  return member;
}
