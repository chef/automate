import { ChefLogo } from './chef-logo';

describe('chef-logo', () => {
  it('throws an error if the company is not known', async () => {
    const el = new ChefLogo();
    const func = el.validateCompany.bind(el, 'Bobo T Clown Co', '');
    expect(func).toThrow('The supplied name "Bobo T Clown Co" is invalid!');
  });
});
