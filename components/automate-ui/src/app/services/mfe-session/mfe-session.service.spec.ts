import { TestBed } from '@angular/core/testing';

import { MfeSessionService } from './mfe-session.service';

describe('MfeSessionService', () => {
  let service: MfeSessionService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [MfeSessionService]
    });
    service = TestBed.inject(MfeSessionService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
