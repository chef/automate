import { TestBed } from '@angular/core/testing';
import { ReportQueryService } from './report-query.service';

describe('ReportQueryService', () => {
  let service: ReportQueryService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        ReportQueryService
      ]
    });

    service = TestBed.get(ReportQueryService);
  });

  describe('addFilter()', () => {
    it('adds the given filter to the filters array', () => {
      const filter = {type: 'Node', value: 'iz-a-node'};
      const filter1 = {type: 'Node', value: 'node'};
      const filter2 = {type: 'Profile', value: 'title'};
      service.addFilter(filter);
      expect(service.filters.getValue()).toContain(filter);
      service.addFilter(filter1);
      expect(service.filters.getValue()).toContain(filter1);
      service.addFilter(filter2);
      expect(service.filters.getValue()).toContain(filter2);
    });

    describe('when the filter type is profile', () => {
      it('appends the version to the text to display the version of the profile', () => {
        const filter = { type: {name: 'profile'} , value: { text: 'title', version: '1.2'} };
        service.addFilter(filter);
        expect(service.filters.getValue()).toContain(
          { type: {name: 'profile'} , value: { text: 'title, v1.2', version: '1.2'} }
        );
      });
    });

    it('does not add a filter if it already exists', () => {
      const filter =  {type: 'Node', value: 'iz-a-node'};
      const filter1 = {type: 'Node', value: 'iz-a-node'};
      const filter2 = {type: 'Profile', value: 'title'};
      service.addFilter(filter);
      service.addFilter(filter1);
      service.addFilter(filter2);
      expect(service.filters.getValue()).toContain(
        {type: 'Node', value: 'iz-a-node'}, {type: 'Profile', value: 'title'}
      );
    });

    describe('when the type is profile or node', () => {
      it('does not add the filter is there is already a filter with matching value.id', () => {
        const filter =  {type: {name: 'node'}, value: {text: 'some-node', id: '123'}};
        const filter1 = {type: {name: 'node'}, value: {text: 'SoME-nOde', id: '123'}};
        const filter2 = {
          type: {name: 'profile'},
          value: {text: 'Dev Sec', id: '679', version: '1.2'}
        };
        const filter3 = {
          type: {name: 'profile'},
          value: {text: 'dev-sec', id: '679', version: '1.2'}
        };
        service.addFilter(filter);
        service.addFilter(filter1);
        service.addFilter(filter2);
        service.addFilter(filter3);
        expect(service.filters.getValue()).toContain(
          {type: {name: 'node'}, value: {text: 'some-node', id: '123'}},
          {type: {name: 'profile'}, value: {text: 'Dev Sec, v1.2', id: '679', version: '1.2'}}
        );
      });
    });
  });

  describe('removeFilter()', () => {
    it('removes the given filter from the filters array', () => {
      const filter = {type: 'Node', value: 'iz-a-node'};
      service.removeFilter(filter);
      expect(service.filters.getValue()).not.toContain(filter);
    });
  });

  describe('clearFilters()', () => {
    it('clears the value of filters', () => {
      service.addFilter({type: 'Node', value: 'iz-a-node'});
      service.clearFilters();
      expect(service.filters.getValue())
        .not.toContain({type: 'Node', value: 'iz-a-node'});
    });

    it('does not clear the date filter', () => {
      const filter = {type: 'Node', value: 'iz-a-node'};
      const dateFilter = {end_time: new Date()};
      service.filters.next([
        dateFilter,
        filter
      ]);

      service.clearFilters();

      expect(service.filters.getValue()).toEqual([dateFilter]);
    });
  });
});
