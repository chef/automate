import { TestBed, inject } from '@angular/core/testing';

import { SelectBoxService } from './select-box.service';

describe('SelectBoxService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [SelectBoxService]
    });
  });

  it('should be created', inject([SelectBoxService], (service: SelectBoxService) => {
    expect(service).toBeTruthy();
  }));
});
