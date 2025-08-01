import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { RevisionIdComponent } from './revision-id.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { MockChefError, MockChefFormField, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefToolbar, MockChefTr } from 'app/testing/mock-components';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { By } from '@angular/platform-browser';
import { Revision } from 'app/entities/revisions/revision.model';
import { GetRevisionsSuccess } from 'app/entities/revisions/revision.action';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('RevisionIdComponent', () => {
  let component: RevisionIdComponent;
  let fixture: ComponentFixture<RevisionIdComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        RevisionIdComponent
      ],
      providers: [
        FeatureFlagsService,
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks }),
        MockChefTh,
        MockChefTd,
        MockChefError,
        MockChefFormField,
        MockChefHeading,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockComponent({ selector: 'mat-select' }),
        MockComponent({ selector: 'mat-option' }),
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockChefTable,
        MockChefThead,
        MockChefTbody,
        MockChefTr,
        MockChefTh,
        MockChefTd,
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RevisionIdComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('revision id list', () => {
    let store: Store<NgrxStateAtom>;
    const availableRevisions: Revision[] = [{
        revision_id: '2.3.12'
      }
    ];
    const emptyRevisions: Revision[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the policyfile list', () => {
      store.dispatch(new GetRevisionsSuccess({revisions: availableRevisions}));
      expect(component.revisions.length).not.toBeNull();
      expect(element.query(By.css('.empty-section'))).toBeNull();
    });

    it('show no preview image', () => {
      store.dispatch(new GetRevisionsSuccess({revisions: emptyRevisions}));
      expect(component.revisions.length).toBe(0);
    });
  });
});
