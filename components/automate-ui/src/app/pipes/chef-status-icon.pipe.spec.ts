import { ChefStatusIconPipe } from './chef-status-icon.pipe';

describe('ChefStatusIconPipe', () => {
  let pipe: ChefStatusIconPipe;

  beforeEach(() => pipe = new ChefStatusIconPipe());

  describe('#transform', () => {
    it('should convert success to check_circle', () => {
      expect(pipe.transform('success')).toEqual('check_circle');
    });

    it('should convert failure to warning', () => {
      expect(pipe.transform('failure')).toEqual('warning');
    });

    it('should convert missing to help', () => {
      expect(pipe.transform('missing')).toEqual('help');
    });

    it('should default to circle/lens', () => {
      expect(pipe.transform('total')).toEqual('lens');
    });
  });
});
