import { UploadService } from './upload.service';

class MockChefSessionService {
  enterprise = 'TestEnt';
  username = 'TestUser';
  token = 'TestToken';
  public logout(): void {}
}

describe('UploadService', () => {
  let service: UploadService;
  let mockChefSessionService;

  beforeEach(() => {
     mockChefSessionService = new MockChefSessionService();
     service = new UploadService(mockChefSessionService);
  });
  describe('getProgress', () => {
    it('returns the correct object', () => {
      expect(service.getProgress('file', 25, 1, 'response'))
        .toEqual({
          'name': 'file',
          'percent': 25,
          'status': 1,
          'response': 'response'
        });
    });
  });

  describe('estimateContentType', () => {
    it('returns the expected type', () => {
      expect(service.estimateContentType(null, 'file.tgz')).toEqual('application/gzip');
      expect(service.estimateContentType(null, 'file.zip')).toEqual('application/zip');
    });
  });
});
