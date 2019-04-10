import { TestBed, inject } from '@angular/core/testing';

import { NodeDetailsService } from './node-details.service';

describe('NodeDetailsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [NodeDetailsService]
    });
  });

  it('should be created', inject([NodeDetailsService], (service: NodeDetailsService) => {
    expect(service).toBeTruthy();
  }));
});
