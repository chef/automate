import { TestBed } from '@angular/core/testing';

import { LicenseUsageService } from './license-usage.service';

describe('LicenseUsageService', () => {
  let service: LicenseUsageService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(LicenseUsageService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
