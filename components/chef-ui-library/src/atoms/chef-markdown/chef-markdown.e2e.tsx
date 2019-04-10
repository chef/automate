import { newE2EPage } from '@stencil/core/testing';

describe('chef-markdown', () => {
  const html = `<chef-markdown></chef-markdown>`;

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-markdown');
    expect(element).toHaveClass('hydrated');
  });

  it('displays parsed markdown text', async () => {
    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-markdown');
    element.setProperty('text', '# Test Heading');

    await page.waitForChanges();

    const content = await element.find('div');
    expect(content.innerHTML.trim())
      .toEqual('<h1 id="test-heading">Test Heading</h1>');
  });

  it('displays code blocks as `chef-snippet`', async () => {
    const code = 'const x = "foo"';
    const lang = 'js';
    const text = [
      '```' + lang,
      code,
      '```'
    ].join('\n');

    const page = await newE2EPage();

    await page.setContent(html);
    const element = await page.find('chef-markdown');
    element.setProperty('text', text);

    await page.waitForChanges();

    const content = await element.find('chef-snippet');

    expect(content).not.toBeNull();
  });
});
