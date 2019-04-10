import { ServiceStatusIconPipe } from './service-status-icon.pipe';

describe('ServiceStatusIconPipe', () => {
  let pipe: ServiceStatusIconPipe;

  beforeEach(() => pipe = new ServiceStatusIconPipe());

  describe('#transform', () => {
    it('should convert success to check_circle', () => {
      expect(pipe.transform('OK')).toEqual('check_circle');
    });

    it('should convert failure to warning', () => {
      expect(pipe.transform('CRITICAL')).toEqual('warning');
    });

    it('should convert unknown to warning', () => {
      expect(pipe.transform('UNKNOWN')).toEqual('warning');
    });

    it('should convert missing to help', () => {
      expect(pipe.transform('DEPLOYING')).toEqual('help');
    });

    it('should default to circle/lens', () => {
      expect(pipe.transform('TOTAL')).toEqual('lens');
    });
  });
});
