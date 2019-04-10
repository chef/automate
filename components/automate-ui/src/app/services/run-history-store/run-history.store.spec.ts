import { TestBed, inject } from '@angular/core/testing';

import { RunHistoryStore } from './run-history.store';

describe('RunHistoryStoreService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [RunHistoryStore]
    });
  });

  it('should be created', inject([RunHistoryStore], (service: RunHistoryStore) => {
    expect(service).toBeTruthy();
  }));
});
