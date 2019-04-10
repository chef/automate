import { browser } from 'protractor';

describe('Signin Process', () => {
  // tslint:disable-next-line:max-line-length
  const valid_id_token = 'eyJhbGciOiJSUzI1NiIsImtpZCI6ImJkOWY5ZGFkZDc4ZDEyOWFlN2I2ODZhZTU0NjJhOWYzY2JmMDY1MTUifQ.eyJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjQyMDAvZGV4Iiwic3ViIjoiQ2cwd0xUTTROUzB5T0RBNE9TMHdFZ1J0YjJOciIsImF1ZCI6ImF1dG9tYXRlLXVpIiwiZXhwIjoxNTA5NzIwMTgzLCJpYXQiOjE1MDk2MzM3ODMsImF0X2hhc2giOiJ4ck1fTXNmLUd1dmY1dzRGeWY1THVRIiwiZW1haWwiOiJraWxnb3JlQGtpbGdvcmUudHJvdXQiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZ3JvdXBzIjpbImF1dGhvcnMiXSwibmFtZSI6IktpbGdvcmUgVHJvdXQifQ.CsBjk47MdwpkneBsbc9NEIx8TskokPDrd3Bp-C4GhcdC-eZH-vOKBnRytMi7_GcOchevo7KCmwjzZllC-AgJMd7b5SBWVjDzLQuS8D9zIX_t_vf3c_wwl4R_fYjBiO7wmm3u-VQGCmxX4UjqyfzWCT-FYwLH5WctVusM3bdlAF0FiLndkmiyAaNFbxMznlDwmrys39in4oV9srxZnXrK-ydlhpJJzETrwBVmAhDzKJO62GC6WcFQYFeQ0Dtb6eBSFaRBi7LmM5TUT_qcIW-LRGcfa7h2DfifKEgCFuv6QjUXb8B7fxRZNMQyAcoVV9qZK8Nd51l-anDD1PI4J12hyw';

  function validSigninPathWithState(state: string = ''): string {
    return `/signin#id_token=${valid_id_token}&state=${state}`;
  }

  // TODO 2017/11/16 sr: e2e-test this! Currently, it's hard because of the way
  // ChefSession's constructor uses the environment and we can't change settings
  // of the environment here. Need to find a better solution.
  // describe('with no session information', () => {
  //   beforeEach(() => {
  //     browser.executeScript('window.localStorage.clear();');
  //     fakeServer().get('/dex/auth').many().reply(200);
  //   });
  //   it('redirects to dex', () => {
  //     browser.get('/');
  //     expect(browser.getCurrentUrl()).toMatch(/\/dex\/auth$/);
  //   });
  // });

  beforeAll(() => {
    browser.waitForAngularEnabled(false);
  });

  describe('when the user logs in', () => {
    it('redirects to / when nothing else is passed in state', () => {
      browser.get(validSigninPathWithState());
      expect(browser.getCurrentUrl()).toMatch(/\/event-feed$/);
    });

    it('redirects to /settings when passed in state', () => {
      browser.get(validSigninPathWithState('%2fsettings'));
      expect(browser.getCurrentUrl()).toMatch(/\/settings$/);
    });

    it('stores session info in localStorage from ID token', () => {
      browser.get(validSigninPathWithState());
      const expected = {
        uuid: 'Cg0wLTM4NS0yODA4OS0wEgRtb2Nr',
        fullname: 'Kilgore Trout',
        username: 'kilgore@kilgore.trout',
        groups: ['authors'],
        id_token: valid_id_token
      };
      const actual = browser.executeScript(
        'return window.localStorage.getItem(\'chef-automate-user\');')
        .then(JSON.parse);
      expect(actual).toEqual(expected);
    });
  });
});
