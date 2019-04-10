import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CookieModule } from 'ngx-cookie';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { NodesAddComponent } from './nodes-add.component';

describe('NodesAddComponent', () => {
  let fixture: ComponentFixture<NodesAddComponent>;
  let component: NodesAddComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        FormsModule,
        ReactiveFormsModule,
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      declarations: [
        NodesAddComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(NodesAddComponent);
    component = fixture.componentInstance;
  });

  beforeEach(() => {
    component.ngOnInit();
  });

  describe('navToStep()', () => {
    it('sets activeStep to provided number', () => {
      component.activeStep = 1;
      component.navToStep(2);
      expect(component.activeStep).toEqual(2);
    });
  });

  describe('stepIsActive()', () => {
    it('returns boolean for active state of provided step', () => {
      component.activeStep = 1;
      expect(component.stepIsActive(1)).toEqual(true);
      expect(component.stepIsActive(2)).toEqual(false);
    });
  });

  describe('stepIsValid()', () => {
    it('returns boolean for form group validity of provided step', () => {
      expect(component.stepIsValid(1))
        .toEqual(component.form.controls['wizardStep1'].valid);
    });
  });
});
