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

  describe('FilterTitle', () => {
    it('set and retrieve', () => {
      const type = 'node_id';
      const title = 'iz-a-node';
      const id = 'id';
      service.setFilterTitle(type, id, title);
      expect(service.getFilterTitle(type, id)).toEqual(title);
    });

    it('update existing type and id', () => {
      const type = 'node_id';
      const title1 = 'iz-a-node';
      const id = 'id';
      service.setFilterTitle(type, id, title1);
      expect(service.getFilterTitle(type, id)).toEqual(title1);

      const title2 = 'iz-a-node-2';
      service.setFilterTitle(type, id, title2);
      expect(service.getFilterTitle(type, id)).toEqual(title2);
    });

    it('set and retrieve same ID different types', () => {
      const nodeType = 'node_id';
      const profileType = 'profile_id';
      const nodeTitle = 'iz-a-node';
      const profileTitle = 'iz-a-profile';
      const id = 'id';
      service.setFilterTitle(nodeType, id, nodeTitle);
      service.setFilterTitle(profileType, id, profileTitle);

      expect(service.getFilterTitle(nodeType, id)).toEqual(nodeTitle);
      expect(service.getFilterTitle(profileType, id)).toEqual(profileTitle);
    });
  });
});
