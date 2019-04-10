import { newE2EPage } from '@stencil/core/testing';

describe('chef-separator', () => {
  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent('<chef-separator><chef-button>Content</chef-button></chef-separator>');
    const element = await page.find('chef-separator');
    expect(element).toHaveClass('hydrated');
  });

  it('displays slotted content', async () => {
    const page = await newE2EPage();

    await page.setContent('<chef-separator><chef-button>Content</chef-button></chef-separator>');
    const element = await page.find('chef-separator');
    expect(element.textContent).toEqual('Content');
  });
});
