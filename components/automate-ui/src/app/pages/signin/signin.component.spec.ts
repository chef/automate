import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { SigninComponent } from './signin.component';
import { MockComponent } from 'ng2-mock-component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ReplaySubject } from 'rxjs';

class MockActivatedRoute {
  private subject = new ReplaySubject<string>();

  constructor(initialFragment: string = '') {
    this.setFragment(initialFragment);
  }

  readonly fragment = this.subject.asObservable();
  readonly snapshot = {};

  setFragment(frag: string) {
    this.subject.next(frag);
  }
}
function setCookie(key: string, value: string): void {
  document.cookie = `${key}=${value}; Path=/;`;
}

function deleteIdTokenFromCookie(): void {
  // Expire id_token cookie once it's set in localStorage
  document.cookie = `id_token=; Path=/; Expires=${new Date().toUTCString()};`;
}

describe('SigninComponent', () => {
  let component: SigninComponent;
  let fixture: ComponentFixture<SigninComponent>;
  let chefSessionService: ChefSessionService;
  let activatedRoute: MockActivatedRoute;

  const valid_id_token_key = 'id_token';
  // tslint:disable-next-line:max-line-length
  const valid_id_token = 'eyJhbGciOiJSUzI1NiIsImtpZCI6ImJkOWY5ZGFkZDc4ZDEyOWFlN2I2ODZhZTU0NjJhOWYzY2JmMDY1MTUifQ.eyJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjQyMDAvZGV4Iiwic3ViIjoiQ2cwd0xUTTROUzB5T0RBNE9TMHdFZ1J0YjJOciIsImF1ZCI6ImF1dG9tYXRlLXVpIiwiZXhwIjoxNTA5NzIwMTgzLCJpYXQiOjE1MDk2MzM3ODMsImF0X2hhc2giOiJ4ck1fTXNmLUd1dmY1dzRGeWY1THVRIiwiZW1haWwiOiJraWxnb3JlQGtpbGdvcmUudHJvdXQiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZ3JvdXBzIjpbImF1dGhvcnMiXSwibmFtZSI6IktpbGdvcmUgVHJvdXQifQ.CsBjk47MdwpkneBsbc9NEIx8TskokPDrd3Bp-C4GhcdC-eZH-vOKBnRytMi7_GcOchevo7KCmwjzZllC-AgJMd7b5SBWVjDzLQuS8D9zIX_t_vf3c_wwl4R_fYjBiO7wmm3u-VQGCmxX4UjqyfzWCT-FYwLH5WctVusM3bdlAF0FiLndkmiyAaNFbxMznlDwmrys39in4oV9srxZnXrK-ydlhpJJzETrwBVmAhDzKJO62GC6WcFQYFeQ0Dtb6eBSFaRBi7LmM5TUT_qcIW-LRGcfa7h2DfifKEgCFuv6QjUXb8B7fxRZNMQyAcoVV9qZK8Nd51l-anDD1PI4J12hyw';


  beforeEach(() => {
    activatedRoute = new MockActivatedRoute('');
  });

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule.withRoutes([{ path: '**', component: SigninComponent }]) // match all
      ],
      declarations: [
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked'] }),
        SigninComponent
      ],
      providers: [
        // Note: the [routerLink] in the template requires snapshot to exist
        { provide: ActivatedRoute, useValue: activatedRoute },
        { provide: ChefSessionService, useClass: MockChefSessionService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SigninComponent);
    chefSessionService = TestBed.inject(ChefSessionService);
    component = fixture.componentInstance;
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('ngOnInit', () => {
    describe('sets error', () => {
      it('to true when fragment is missing', () => {
        activatedRoute.setFragment(null);
        fixture.detectChanges();

        expect((component as any).error).toEqual(true);
      });

      it('to true when id_token does not exist in cookie', () => {
        const [id_token, state] = component.idTokenAndStateFromCookieAndFragment('');
        deleteIdTokenFromCookie();
        fixture.detectChanges();
        expect(id_token).toEqual(null);
        expect(state).toEqual(null);

        expect((component as any).error).toEqual(true);
      });

      it('to false when id_token exists in cookie', () => {
        setCookie(valid_id_token_key, valid_id_token);
        const [id_token, state] = component.idTokenAndStateFromCookieAndFragment('state=');
        fixture.detectChanges();
        expect(id_token).toBe(valid_id_token);
        expect(state).toBe('');

        expect((component as any).error).toEqual(false);
      });

      it('to true when id token from cookie cannot be decoded', () => {
        const not_a_jwt_value = 'NOTAJWT';
        setCookie(valid_id_token_key, not_a_jwt_value);
        fixture.detectChanges();
        const [id_token, state] = component.idTokenAndStateFromCookieAndFragment('state=');
        fixture.detectChanges();
        expect(id_token).toBe(not_a_jwt_value);
        expect(state).toBe('');

        expect((component as any).error).toEqual(true);
      });
    });

    it('stores information on success', () => {
      activatedRoute.setFragment('state=foo');
      setCookie(valid_id_token_key, valid_id_token);
      fixture.detectChanges();
      const [id_token, state] = component.idTokenAndStateFromCookieAndFragment('state=foo');
      fixture.detectChanges();

      const id = (component as any).id;
      expect(id.sub).toEqual('Cg0wLTM4NS0yODA4OS0wEgRtb2Nr');
      expect(id.name).toEqual('Kilgore Trout');
      expect(id.email).toEqual('kilgore@kilgore.trout');
      expect(id.groups).toEqual(['authors']);
      expect((component as any).idToken).toEqual(id_token);
      expect(state).toEqual('foo');
    });
  });

  describe('#setSession', () => {

    it('sets the session in chef-session from its state', () => {
      const mock = spyOn(chefSessionService, 'setSession');
      (component as any).id = {
        sub: 'Cg0wLTM4NS0yODA4OS0wEgRtb2Nr',
        name: 'Kilgore Trout',
        email: 'kilgore@kilgore.trout',
        groups: ['authors'],
        federated_claims: {
          connector_id: 'local'
        }
      };
      (component as any).idToken = valid_id_token;

      component.setSession();

      expect(mock).toHaveBeenCalledWith(
        'Cg0wLTM4NS0yODA4OS0wEgRtb2Nr',
        'Kilgore Trout',
        'kilgore@kilgore.trout',
        valid_id_token,
        ['authors'],
        true);
    });

    it('sets the SAML session in chef-session from its state', () => {
      const mock = spyOn(chefSessionService, 'setSession');
      (component as any).id = {
        sub: 'Cg0wLTM4NS0yODA4OS0wEgRtb2Nr',
        name: 'Kilgore Trout',
        email: 'kilgore@kilgore.trout',
        groups: ['authors'],
        federated_claims: {
          connector_id: 'saml'
        }
      };
      (component as any).idToken = valid_id_token;

      component.setSession();

      expect(mock).toHaveBeenCalledWith(
        'Cg0wLTM4NS0yODA4OS0wEgRtb2Nr',
        'Kilgore Trout',
        'kilgore@kilgore.trout',
        valid_id_token,
        ['authors'],
        true);
    });
  });

  describe('#pathFromState', () => {
    // This case is not special in terms of processing, but the most common
    // one for this component.
    it('returns "" when it is passed empty state', () => {
      expect(component.pathFromState('')).toEqual('');
    });

    // Note: I believe this cannot happen. Angular freaks out if this happens
    // before we can do anything about it.
    it('returns "/" when it is passed state that is not URI-encoded', () => {
      spyOn(console, 'error');
      expect(component.pathFromState('%3')).toEqual('/');
      expect(console.error).toHaveBeenCalled();
    });

    it('routes to "/foo" when it is passed state that is properly URI-encoded', () => {
      expect(component.pathFromState('%2ffoo')).toEqual('/foo');
    });
  });

  describe('#idTokenFromCookieAndstateFromFragment', () => {
    // empty state: not special, but common
    it('returns empty id_token and matches an empty state', () => {
      const actual = component.idTokenAndStateFromCookieAndFragment('state=');
      expect(actual).toEqual([null, '']);
    });

    it('if the match fails returns nothing', () => {
      const actual = component.idTokenAndStateFromCookieAndFragment('notwhatweexpect');
      expect(actual).toEqual([null, null]);
    });

    it('expects the ID token to be non-empty', () => {
      const actual = component.idTokenAndStateFromCookieAndFragment('state=%2Fnodes');
      expect(actual).toEqual([null, '%2Fnodes']);
    });
  });
});
