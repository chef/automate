import { TestBed } from '@angular/core/testing';
import { provideMockActions } from '@ngrx/effects/testing';

import { Observable } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';

import { ServiceGroupsEffects } from './service-groups.effects';
import { ServiceGroupsRequests } from './service-groups.requests';

describe('ServiceGroupsEffects', () => {
  let effects: ServiceGroupsEffects;
  let actions: Observable<any>;

  const initialState = {};

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule,
        StoreModule.forRoot(ngrxReducers, {initialState, runtimeChecks}),
      ],
      providers: [
        ServiceGroupsEffects,
        ServiceGroupsRequests,
        provideMockActions(() => actions)
      ]
    });

    effects = TestBed.inject(() => actions);
  });

  describe('Deleting a Service', () => {
    it('test', () => {
      // getting a NullInjectorError on line 33....?
    });
  });
});

