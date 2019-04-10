import { UploadService } from './upload.service';

class MockChefSessionService {
  enterprise = 'TestEnt';
  username = 'TestUser';
  token = 'TestToken';
  public logout(): void {}
}

describe('UploadService', () => {
  beforeEach(() => {
     this.mockChefSessionService = new MockChefSessionService();
     this.service = new UploadService(this.mockChefSessionService);
  });
  describe('getProgress', () => {
    it('returns the correct object', () => {
      expect(this.service.getProgress('file', 'percent', 'status', 'response'))
        .toEqual({
          'name': 'file',
          'percent': 'percent',
          'status': 'status',
          'response': 'response'
        });
    });
  });

  describe('estimateContentType', () => {
    it('returns the expected type', () => {
      expect(this.service.estimateContentType(null, 'file.tgz')).toEqual('application/gzip');
      expect(this.service.estimateContentType(null, 'file.zip')).toEqual('application/zip');
    });
  });
});
