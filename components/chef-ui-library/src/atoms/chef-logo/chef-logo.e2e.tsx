import { newE2EPage } from '@stencil/core/testing';

describe('chef-logo', () => {
  const html = '<chef-logo company="Chef Software"></chef-logo>';

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-logo');
    expect(element).toHaveClass('hydrated');
  });

  it('produces an img tag', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('img');
    expect(element).toBeDefined();
  });

  it('sets the image alt to the company name', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const content = await page.find('img');

    expect(content.getAttribute('alt')).toEqual('Chef Software logo');
  });

  it('sets the image src to the correct image from the company data', async () => {
    const page = await newE2EPage();

    await page.setContent('<chef-logo company="Slack"></chef-logo>');
    const content = await page.find('img');

    expect(content.getAttribute('src')).toEqual('/assets/logos/slack.svg');
  });

  it('shows an unknown logo by default', async () => {
    const page = await newE2EPage();

    await page.setContent('<chef-logo></chef-logo>');
    const content = await page.find('img');

    expect(content.getAttribute('alt')).toEqual('Unknown logo');
    expect(content.getAttribute('src')).toEqual('/assets/logos/Unknown.svg');
  });
});
