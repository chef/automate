import { TestBed } from '@angular/core/testing';
import { provideMockActions } from '@ngrx/effects/testing';
import { Observable, of } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { StoreModule } from '@ngrx/store';
import {
    ngrxReducers,
    runtimeChecks
} from 'app/ngrx.reducers';

import { ProjectEffects } from './project.effects';
import { DeleteProjectSuccess } from './project.actions';
import { LoadOptions } from 'app/services/projects-filter/projects-filter.actions';
import { Type } from 'app/entities/notifications/notification.model';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { ProjectRequests } from './project.requests';

describe('ProjectEffects', () => {
  let effects: ProjectEffects;
  let actions$: Observable<any>;
  const initialState = { };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks })
      ],
      providers: [
        ProjectEffects,
        ProjectRequests,
        provideMockActions(() => actions$)
      ]
    });

    effects = TestBed.inject(ProjectEffects);
  });

  it('DeleteProjectSuccess ensuring the loadOption and CreateNotification are both created',
    done => {
    const id = 'project3';
    const loadOption = new LoadOptions();
    const createNotification = new CreateNotification({
      type: Type.info,
      message: `Deleted project ${id}.`
    });

    actions$ = of(new DeleteProjectSuccess({id}));

    let loadOptionSeen = false;
    let createNotificationSeen = false;
    let numberOfActionsSeen = 0;

    // The subscribe should be called with two actions of loadOption and CreateNotification
    effects.deleteProjectSuccess$.subscribe(result => {
      numberOfActionsSeen += 1;
      if (result.type === loadOption.type ) {
        loadOptionSeen = true;
      } else if (result.type === createNotification.type &&
        result.payload.message === createNotification.payload.message) {
        createNotificationSeen = true;
      } else {
        fail('The action returned was not correct. action: ' + JSON.stringify(result));
      }

      if (numberOfActionsSeen > 2) {
        fail('Only two actions should be returned');
      } else if ( numberOfActionsSeen === 2 ) {
        expect(loadOptionSeen).toEqual(true);
        expect(createNotificationSeen).toEqual(true);
        done();
      }
    });
  });
});
