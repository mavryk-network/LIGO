// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const singleTheme = require('prism-react-renderer/themes/duotoneLight');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'LIGO',
  tagline: 'LIGO is a friendly smart contract language for Tezos',
  url: 'https://ligolang.org',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.svg',
  projectName: 'ligo',
  organizationName: 'Marigold',

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  plugins: ["@ligo/syntax"],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: '../docs',
          sidebarPath: require.resolve('./sidebars.js'),
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          // editUrl:
          //   'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        gtag: {
          trackingID: 'G-V5S4SDLK4Z'
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      announcementBar: {
        id: 'support_us',
        content:
          '<b>Ligo v1 is now available ! Check the migration guide : https://ligolang.org/docs/next/faq/v1-migration-guide ! </b>',
        backgroundColor: '#0e74ff',
        textColor: '#efefef',
        isCloseable: false,
      },
      navbar: {
        logo: {
          alt: 'LIGO Logo',
          src: 'img/logo/logo.svg',
          srcDark: 'img/logo/logo-night.svg'
        },
        items: [
          { type: 'docsVersionDropdown', position: 'left' },
          { to: 'docs/intro/introduction', label: 'Docs', position: 'left', target: '_self' },
          { to: 'docs/reference/toplevel', label: 'API', position: 'left' },
          { to: 'docs/faq/intro', label: 'FAQ', position: 'left' },
          { to: 'https://packages.ligolang.org/packages', label: 'Registry', position: 'right' },
          { to: 'https://ide.ligolang.org/', label: 'Web IDE', position: 'right' }
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Tooling',
            items: [
              {
                label: 'Taqueria',
                to: 'docs/intro/installation',
              },
              {
                label: 'Registry',
                to: 'docs/api/cli-commands',
              },
              {
                label: 'Octez-client',
                to: 'docs/api/cheat-sheet',
              },
              {
                label: 'Templates',
                to: 'docs/api/cheat-sheet',
              }
            ],
          },
          {
            title: 'Need help',
            items: [
              {
                label: 'FAQ',
                href: 'https://tezos.stackexchange.com/questions/tagged/ligo',
              },
              {
                label: 'Discord',
                href: 'https://discord.gg/tezos',
              },
              {
                label: 'Telegram',
                href: 'https://t.me/LigoLang',
              },
              {
                label: 'Twitter',
                href: 'https://twitter.com/LigoLang',
              },
            ],
          },
          {
            title: 'Open source',
            items: [
              {
                label: 'Ligo Gitlab',
                to: 'docs/tutorials/getting-started',
              },
              {
                label: 'Open an issue',
                to: 'https://forum.tezosagora.org/tag/ligo',
              },
              {
                label: 'Website sources',
                href: 'https://gitlab.com/ligolang/ligo',
              }
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} LIGO. All rights reserved.`,
      },
      image: 'img/logo/logo.png',
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        singleTheme: singleTheme
      },
      algolia: {
        // The application ID provided by Algolia
        appId: 'ZJTW93II01',

        // Public API key: it is safe to commit it
        apiKey: '666cd6151b57b31964fece17ad094ba9',

        indexName: 'ligolang',

        // Optional: see doc section below
        contextualSearch: true,

        // Optional: Algolia search parameters
        searchParameters: {},

        // Optional: path for search page that enabled by default (`false` to disable it)
        searchPagePath: 'search',

      },
    })
};

module.exports = config;
