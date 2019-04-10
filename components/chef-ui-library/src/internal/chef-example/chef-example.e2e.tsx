import { newE2EPage } from '@stencil/core/testing';

describe('chef-example', () => {
  const code = '<h1>Example markup</h1>';
  const html = `<chef-example code="${code}"></chef-example>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-example');
    expect(element).toHaveClass('hydrated');
  });

  it('displays code snippet of the provided example', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const snippet = await page.find('.example-code');
    expect(snippet).toBeTruthy();
  });

  it('displays the provided example', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const snippet = await page.find('.example-display');
    expect(snippet).toBeTruthy();
  });
});
