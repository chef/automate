import { E2EElement, E2EPage, newE2EPage } from '@stencil/core/testing';

describe('chef-tab-selector', () => {
  let page: E2EPage;
  let element: E2EElement;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`
      <chef-tab-selector value='opt2'>
        <chef-option value='opt1'>Option 1</chef-option>
        <chef-option value='opt2'>Option 2</chef-option>
        <chef-option value='opt3'>Option 3</chef-option>
      </chef-tab-selector>
    `);
    element = await page.find('chef-tab-selector');
  });

  it('renders', async () => {
    expect(element).toHaveClass('hydrated');
  });

  describe('when clicked', () => {
    it('sets value to value of clicked chef-option', async () => {
      const option = await element.find('chef-option[value="opt1"]');
      await option.click();
      await page.waitForChanges();
      expect(await element.getProperty('value')).toEqual('opt1');
    });
  });
});
