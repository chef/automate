import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { StoreModule } from '@ngrx/store';
import * as moment from 'moment';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { JobsListComponent } from './jobs-list.component';

describe('JobsListComponent', () => {
  let fixture: ComponentFixture<JobsListComponent>;
  let component: JobsListComponent;
  let router: Router;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        StoreModule.forRoot({}),
        HttpClientTestingModule
      ],
      declarations: [
        JobsListComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(JobsListComponent);
    component = fixture.componentInstance;
    router = TestBed.get(Router);
  });

  describe('timeFromNow()', () => {
    describe('when time is beginning of time', () => {
      const time = '0001-01-01T00:00:00.000Z';

      it('returns `-`', () => {
        expect(component.timeFromNow(time)).toEqual('-');
      });
    });

    describe('when time is later than beginning of time', () => {
      const time = '2018-01-01T00:00:00.000Z';

      it('returns `moment.fromNow`', () => {
        expect(component.timeFromNow(time)).toEqual(moment(time).fromNow());
      });
    });
  });

  describe('isJobReport()', () => {
    describe('when job has an empty recurrence value', () => {
      it('returns `true`', () => {
        const job = { recurrence: '' };
        expect(component.isJobReport(job)).toEqual(true);
      });
    });

    describe('when job has a non-empty recurrence value', () => {
      it('returns `false`', () => {
        const job = { recurrence: 'DTSTART=20180511T095200Z' };
        expect(component.isJobReport(job)).toEqual(false);
      });
    });
  });

  describe('viewReport()', () => {
    it('correct end date and job ID', () => {
      spyOn(router, 'navigate');
      const time = '2018-01-01T00:00:00.000Z';
      component.viewReport('fake_id', time);
      expect(router.navigate).toHaveBeenCalledWith(['/compliance', 'reports', 'overview'],
        {queryParams: {job_id: 'fake_id', end_time: moment(time).format('YYYY-MM-DD')}});
    });

    it('null end date', () => {
      spyOn(router, 'navigate');
      component.viewReport('fake_id', null);
      expect(router.navigate).toHaveBeenCalledWith(['/compliance', 'reports', 'overview'],
        {queryParams: {job_id: 'fake_id'}});
    });

    it('beginning of time end date', () => {
      spyOn(router, 'navigate');
      component.viewReport('fake_id', new Date(0));
      expect(router.navigate).toHaveBeenCalledWith(['/compliance', 'reports', 'overview'],
        {queryParams: {job_id: 'fake_id'}});
    });
  });
});
