import { ChefOption } from './chef-option';

describe('chef-option', () => {

  it('exists', () => {
    expect(new ChefOption()).toBeTruthy();
  });

  it('gets a unique optionId', () => {
    const option1 = new ChefOption();
    const option2 = new ChefOption();

    expect(option1.optionId).not.toEqual(option2.optionId);
  });
});
