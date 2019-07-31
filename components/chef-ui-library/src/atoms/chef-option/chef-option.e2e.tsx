import { newE2EPage } from '@stencil/core/testing';

describe('chef-option', () => {
  it('getContent returns the options content', async () => {
    const page = await newE2EPage();

    await page.setContent('<chef-option><span>Option</span></chef-option>');

    const option = await page.find('chef-option');
    const method = await option.callMethod('getContent');

    expect(method).toContain('<span>Option</span>');
  });
});
