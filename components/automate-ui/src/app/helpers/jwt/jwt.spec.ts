import { Jwt } from './jwt';

describe('Jwt', () => {

  describe('#parseIDToken', () => {
    // Testing the happy path doesn't give us much over what we have already
    // tested in signin-component.spec.ts
    it('returns null when it cannot parse the JWT', () => {
      expect(Jwt.parseIDToken('notajwttoken')).toEqual(null);
    });
  });
});
