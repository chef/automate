import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { SigninComponent } from './signin.component';
import { MockComponent } from 'ng2-mock-component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { Observable, of } from 'rxjs';
import { CallbackResponse, SigninService } from 'app/services/signin/signin.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { LicenseUsageService } from 'app/services/license-usage/license-usage.service';

class ActivatedRouteMock {
  queryParams = new Observable(observer => {
    const urlParams = {
      param1: 'some',
      param2: 'params'
    };
    observer.next(urlParams);
    observer.complete();
  });
}

class SignIn {
  callback() {
    const data: CallbackResponse = {
      state: 'test',
      id_token: 'test'
    };
    return of(data);
  }
}

describe('SigninComponent', () => {
  let component: SigninComponent;
  let fixture: ComponentFixture<SigninComponent>;
  let chefSessionService: ChefSessionService;

  // tslint:disable-next-line:max-line-length
  const valid_id_token = 'eyJhbGciOiJSUzI1NiIsImtpZCI6ImJkOWY5ZGFkZDc4ZDEyOWFlN2I2ODZhZTU0NjJhOWYzY2JmMDY1MTUifQ.eyJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjQyMDAvZGV4Iiwic3ViIjoiQ2cwd0xUTTROUzB5T0RBNE9TMHdFZ1J0YjJOciIsImF1ZCI6ImF1dG9tYXRlLXVpIiwiZXhwIjoxNTA5NzIwMTgzLCJpYXQiOjE1MDk2MzM3ODMsImF0X2hhc2giOiJ4ck1fTXNmLUd1dmY1dzRGeWY1THVRIiwiZW1haWwiOiJraWxnb3JlQGtpbGdvcmUudHJvdXQiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZ3JvdXBzIjpbImF1dGhvcnMiXSwibmFtZSI6IktpbGdvcmUgVHJvdXQifQ.CsBjk47MdwpkneBsbc9NEIx8TskokPDrd3Bp-C4GhcdC-eZH-vOKBnRytMi7_GcOchevo7KCmwjzZllC-AgJMd7b5SBWVjDzLQuS8D9zIX_t_vf3c_wwl4R_fYjBiO7wmm3u-VQGCmxX4UjqyfzWCT-FYwLH5WctVusM3bdlAF0FiLndkmiyAaNFbxMznlDwmrys39in4oV9srxZnXrK-ydlhpJJzETrwBVmAhDzKJO62GC6WcFQYFeQ0Dtb6eBSFaRBi7LmM5TUT_qcIW-LRGcfa7h2DfifKEgCFuv6QjUXb8B7fxRZNMQyAcoVV9qZK8Nd51l-anDD1PI4J12hyw';

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        RouterTestingModule.withRoutes([{ path: '**', component: SigninComponent }]) // match all
      ],
      declarations: [
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked'] }),
        SigninComponent
      ],
      providers: [
        // Note: the [routerLink] in the template requires snapshot to exist
        { provide: ActivatedRoute, useClass: ActivatedRouteMock},
        { provide: ChefSessionService, useClass: MockChefSessionService },
        { provide: SigninService, userClass: SignIn},
        { provide: LicenseUsageService, useValue: jasmine.createSpyObj('ClientRunsStatsService', ['postAnalyticsUsageDataCall', 'sendData']) }
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

      it('to true when id_token does not exist', () => {
        const err = component.setIdAndPath('', '');
        fixture.detectChanges();
        const id_token = (component as any).idToken;
        const path = (component as any).path;
        expect(id_token).toEqual('');
        expect(path).toEqual('');

        expect(err).toEqual(true);
      });

      it('to false when id_token exists', () => {
        const err = component.setIdAndPath(valid_id_token, '');
        fixture.detectChanges();
        const id_token = (component as any).idToken;
        const path = (component as any).path;
        expect(id_token).toBe(valid_id_token);
        expect(path).toBe('');

        expect(err).toEqual(false);
      });

      it('to true when id token cannot be decoded', () => {
        const not_a_jwt_value = 'NOTAJWT';
        const error = component.setIdAndPath(not_a_jwt_value, '');
        fixture.detectChanges();
        const id_token = (component as any).idToken;
        const path = (component as any).path;
        expect(id_token).toBe(not_a_jwt_value);
        expect(path).toBe('');
        expect(error).toEqual(true);
      });
    });

    it('stores information on success', () => {
      component.setIdAndPath(valid_id_token, 'foo');
      fixture.detectChanges();
      const id_token = (component as any).idToken;
      const path = (component as any).path;

      const id = (component as any).id;
      expect(id.sub).toEqual('Cg0wLTM4NS0yODA4OS0wEgRtb2Nr');
      expect(id.name).toEqual('Kilgore Trout');
      expect(id.email).toEqual('kilgore@kilgore.trout');
      expect(id.groups).toEqual(['authors']);
      expect((component as any).idToken).toEqual(id_token);
      expect(path).toEqual('foo');
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

});
