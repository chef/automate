import { newE2EPage } from '@stencil/core/testing';

describe('chef-snippet', () => {
  it('renders', async () => {
    const code = '# hello world';
    const lang = 'md';
    const html = genSnippet(code, lang);

    const page = await newE2EPage();
    await page.setContent(html);

    const element = await page.find('chef-snippet');
    expect(element).toHaveClass('hydrated');
  });

  it('displays highlighted code', async () => {
    const code = '<h1>Example markup</h1>';
    const lang = 'html';
    const html = genSnippet(code, lang);

    const page = await newE2EPage();
    await page.setContent(html);

    const element = await page.find('chef-snippet');
    expect(element.textContent).toEqual(code);
  });

  describe('when `code` prop is empty', () => {
    it('displays nothing', async () => {
      const code = '';
      const lang = 'html';
      const html = genSnippet(code, lang);

      const page = await newE2EPage();
      await page.setContent(html);

      const content = await page.find('pre');
      expect(content).toBeFalsy();
    });
  });

  describe('when grammar rules for provided `lang` are unavailable', () => {
    it('displays provided `code` as plain text', async () => {
      const code = '#>>some code%%';
      const lang = 'unrecognized lang!';
      const html = genSnippet(code, lang);

      const page = await newE2EPage();
      await page.setContent(html);

      const content = await page.find('pre');
      expect(content.textContent).toEqual(code);
    });

    it('does not log error to the console', async () => {
      const code = '#>>some code%%';
      const lang = 'unrecognized lang!';
      const html = genSnippet(code, lang);

      const page = await newE2EPage();
      const consoleSpy = jasmine.createSpy();
      page.on('console', consoleSpy);
      await page.setContent(html);

      expect(consoleSpy).not.toHaveBeenCalled();
    });
  });
});

function genSnippet(code: string, lang: string): string {
  return `<chef-snippet code='${code}' lang='${lang}'></chef-snippet>`;
}
