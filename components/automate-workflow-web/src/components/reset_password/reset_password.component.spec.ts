import { TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { Http, BaseRequestOptions } from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { Observable } from 'rxjs';
import { ResetPasswordComponent } from './reset_password.component';

describe('ResetPasswordComponent', () => {
  let fixture, component, element;
  let backend: MockBackend, http: Http;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        FormsModule
      ],
      declarations: [
        ResetPasswordComponent
      ],

      providers: [

        // Mock the injected Session service
        {
          provide: 'Session',
          useValue: {
            get: () => 'cd'
          }
        },

        // Mock an Http provider. (Insights has a module, MockHttpProviders, that handles this for you.)
        {
          provide: Http, useFactory: (backend, options) => new Http(backend, options),
          deps: [
            MockBackend,
            BaseRequestOptions
          ]
        },
        MockBackend,
        BaseRequestOptions
      ]
    });

    fixture = TestBed.createComponent(ResetPasswordComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    backend = TestBed.get(MockBackend);
    http = TestBed.get(Http);
  });

  describe('submit', () => {
    it('sends the proper request', () => {
      let expectedUrl: string = `/api/v0/e/cd/internal-users/alice/reset-password`;
      spyOn(http, 'post').and.returnValue(Observable.of(true));
      spyOn(component, 'gotoLogin').and.returnValue(true);
      component.token = 'passedtoken';
      component.username = 'alice';
      component.password = 'letmein';
      component.submit();

      let body = { token: 'passedtoken', password: 'letmein' };
      expect(http.post).toHaveBeenCalledWith(expectedUrl, body);
      expect(component.gotoLogin).toHaveBeenCalled();
    });
  });

  describe('submit button', () => {
    it('says Set New Password', () => {
      fixture.detectChanges();
      let button = element.query(By.css('button'));
      expect(button.nativeElement.textContent).toMatch('Set New Password');
    });

  });

  describe('initial state', () => {

    describe('submit button', () => {

      it('is disabled', () => {
        fixture.detectChanges();
        let button = element.query(By.css('button'));
        expect(button.nativeElement.disabled).toBe(true);
      });
    });

    describe('invalidConfirmationPassword', () => {

      it('is false', () => {
        fixture.detectChanges();
        expect(component.invalidConfirmationPassword()).toBe(false);
      });
    });

    describe('validPassword', () => {

      it('is false', () => {
        fixture.detectChanges();
        expect(component.validPassword()).toBe(false);
      });
    });

    describe('success flag', () => {

      it('is false', () => {
        expect(component.success).toBe(false);
      });
    });
  });

  describe('entering the password twice', () => {

    describe('incorrectly', () => {

      beforeEach(() => {
        component.password = 'password1';
        component.passwordConfirmation = 'password2';
      });

      describe('invalidConfirmationPassword', () => {

        it('is true', () => {
          fixture.detectChanges();
          expect(component.invalidConfirmationPassword()).toBe(true);
        });
      });

      describe('validPassword', () => {

        it('is false', () => {
          fixture.detectChanges();
          expect(component.validPassword()).toBe(false);
        });
      });
    });

    describe('correctly', () => {

      beforeEach(() => {
        component.password = 'password1';
        component.passwordConfirmation = 'password1';
      });

      describe('invalidConfirmationPassword', () => {

        it('is false', () => {
          fixture.detectChanges();
          expect(component.invalidConfirmationPassword()).toBe(false);
        });
      });

      describe('validPassword', () => {

        it('is true', () => {
          fixture.detectChanges();
          expect(component.validPassword()).toBe(true);
        });
      });

      describe('clicking submit', () => {

        it('calls submit', () => {
          spyOn(component, 'submit').and.callThrough();
          fixture.detectChanges();
          let button = element.query(By.css('button'));
          button.nativeElement.click();
          expect(component.submit).toHaveBeenCalled();
        });
      });
    });
  });
});
