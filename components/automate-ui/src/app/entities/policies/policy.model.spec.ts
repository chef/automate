import { Policy, Member, Type, stringToMember, policyFromPayload } from './policy.model';

describe('policy model', () => {
  describe('stringToMember', () => {

    it('All members', () => {
      const expectedMember = <Member>{
        name: '*',
        displayName: 'All members',
        displayType: 'All members',
        type: Type.AllMembers
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All users', () => {
      const expectedMember = <Member>{
        name: 'user:*',
        displayName: 'All users',
        displayType: 'All users',
        type: Type.AllUsers
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All local users', () => {
      const expectedMember = <Member>{
        name: 'user:local:*',
        displayName: 'All local users',
        displayType: 'All local users',
        type: Type.AllLocalUsers
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific local user', () => {
      const expectedMember = <Member>{
        name: 'user:local:localuser',
        displayName: 'localuser',
        displayType: 'Local user',
        type: Type.LocalUser
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All LDAP users', () => {
      const expectedMember = <Member>{
        name: 'user:ldap:*',
        displayName: 'All LDAP users',
        displayType: 'All LDAP users',
        type: Type.AllLDAPUsers
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific LDAP user', () => {
      const expectedMember = <Member>{
        name: 'user:ldap:ldapuser',
        displayName: 'ldapuser',
        displayType: 'LDAP user',
        type: Type.LDAPUser
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All SAML users', () => {
      const expectedMember = <Member>{
        name: 'user:saml:*',
        displayName: 'All SAML users',
        displayType: 'All SAML users',
        type: Type.AllSAMLUsers
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific SAML user', () => {
      const expectedMember = <Member>{
        name: 'user:saml:samluser',
        displayName: 'samluser',
        displayType: 'SAML user',
        type: Type.SAMLUser
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All local teams', () => {
      const expectedMember = <Member>{
        name: 'team:local:*',
        displayName: 'All local teams',
        displayType: 'All local teams',
        type: Type.AllLocalTeams
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific local team', () => {
      const expectedMember = <Member>{
        name: 'team:local:localteam',
        displayName: 'localteam',
        displayType: 'Local team',
        type: Type.LocalTeam
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All LDAP teams', () => {
      const expectedMember = <Member>{
        name: 'team:ldap:*',
        displayName: 'All LDAP teams',
        displayType: 'All LDAP teams',
        type: Type.AllLDAPTeams
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific LDAP team', () => {
      const expectedMember = <Member>{
        name: 'team:ldap:ldapteam',
        displayName: 'ldapteam',
        displayType: 'LDAP team',
        type: Type.LDAPTeam
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All SAML teams', () => {
      const expectedMember = <Member>{
        name: 'team:saml:*',
        displayName: 'All SAML teams',
        displayType: 'All SAML teams',
        type: Type.AllSAMLTeams
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific SAML team', () => {
      const expectedMember = <Member>{
        name: 'team:saml:samlteam',
        displayName: 'samlteam',
        displayType: 'SAML team',
        type: Type.SAMLTeam
      };
      compareMemberToStringResult(expectedMember);
    });

    it('All tokens', () => {
      const expectedMember = <Member>{
        name: 'token:*',
        displayName: 'All API tokens',
        displayType: 'All API tokens',
        type: Type.AllTokens
      };
      compareMemberToStringResult(expectedMember);
    });

    it('Specific token', () => {
      const expectedMember = <Member>{
        name: 'token:tokenname',
        displayName: 'tokenname',
        displayType: 'API token',
        type: Type.Token
      };
      compareMemberToStringResult(expectedMember);
    });

    // Unknown cases
    it('notknown', () => {
      const actualMember = stringToMember('notknown');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('notknown:*', () => {
      const actualMember = stringToMember('notknown:*');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('*:unexpected', () => {
      const actualMember = stringToMember('*:unexpected');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user', () => {
      const actualMember = stringToMember('user');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:', () => {
      const actualMember = stringToMember('user:');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:*:extra', () => {
      const actualMember = stringToMember('user:*:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:local', () => {
      const actualMember = stringToMember('user:local');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:wrong:lol', () => {
      const actualMember = stringToMember('user:wrong:lol');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:local:something:extra', () => {
      const actualMember = stringToMember('user:local:something:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:ldap', () => {
      const actualMember = stringToMember('user:ldap');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:ldap:something:extra', () => {
      const actualMember = stringToMember('user:ldap:something:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:saml', () => {
      const actualMember = stringToMember('user:saml');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('user:saml:something:extra', () => {
      const actualMember = stringToMember('user:saml:something:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team', () => {
      const actualMember = stringToMember('team');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:', () => {
      const actualMember = stringToMember('team:');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:*:extra', () => {
      const actualMember = stringToMember('team:*:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:wrong:lol', () => {
      const actualMember = stringToMember('team:wrong:lol');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:local', () => {
      const actualMember = stringToMember('team:local');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:local:something:extra', () => {
      const actualMember = stringToMember('team:local:something:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:ldap', () => {
      const actualMember = stringToMember('team:ldap');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:ldap:something:extra', () => {
      const actualMember = stringToMember('team:ldap:something:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:saml', () => {
      const actualMember = stringToMember('team:saml');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('team:saml:something:extra', () => {
      const actualMember = stringToMember('team:saml:something:extra');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('token', () => {
      const actualMember = stringToMember('token');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('token:', () => {
      const actualMember = stringToMember('token');
      expect(actualMember.type).toEqual(Type.Unknown);
    });

    it('token:too:many', () => {
      const actualMember = stringToMember('token:too:many');
      expect(actualMember.type).toEqual(Type.Unknown);
    });
  });

  describe('policyFromPayload', () => {
    it('removes empty strings from statements', () => {
      const policy = <Policy>{
        members: [],
        statements: [
          {
            effect: 'ALLOW',
            actions: ['*'],
            role: ''
          }
        ]
      };
      const actual = policyFromPayload(policy);
      expect(actual.statements[0].role).toBeUndefined();
      expect(actual.statements[0].actions).toEqual(['*']);
    });

    it('removes empty arrays from statements', () => {
      const policy = <Policy>{
        members: [],
        statements: [
          {
            effect: 'ALLOW',
            actions: [],
            role: 'viewer'
          }
        ]
      };
      const actual = policyFromPayload(policy);
      expect(actual.statements[0].actions).toBeUndefined();
      expect(actual.statements[0].role).toEqual('viewer');
    });
  });
});

function compareMemberToStringResult(expected: Member) {
  const actualMember = stringToMember(expected.name);
  expect(actualMember).toEqual(expected);
}
