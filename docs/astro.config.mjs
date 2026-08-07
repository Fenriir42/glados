// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';

// Load Quant grammar from the extension directory
const quantGrammar = JSON.parse(
	readFileSync(fileURLToPath(new URL('../extension/vscode/quant-highlighter/syntaxes/quant.tmLanguage.json', import.meta.url)), 'utf-8')
);

// Add required properties for Shiki
quantGrammar.name = 'quant';
quantGrammar.aliases = ['qa'];

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'Quant Language',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/Sigmapitech/glados' }],
			expressiveCode: {
				themes: ['github-dark', 'github-light'],
				shiki: {
					langs: [quantGrammar],
				},
			},
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Installation', slug: 'getting-started/installation' },
						{ label: 'Quickstart', slug: 'getting-started/quickstart' },
						{ label: 'Editor Setup', slug: 'getting-started/editor-setup' },
					],
				},
				{
					label: 'Language',
					items: [
						{ label: 'Variables & Types', slug: 'language/variables-and-types' },
						{ label: 'Expressions & Operators', slug: 'language/expressions' },
						{ label: 'Control Flow', slug: 'language/control-flow' },
						{ label: 'Functions', slug: 'language/functions' },
						{ label: 'Structs & Methods', slug: 'language/structs' },
						{ label: 'Tuples', slug: 'language/tuples' },
						{ label: 'Enums', slug: 'language/enums' },
						{ label: 'Interfaces', slug: 'language/interfaces' },
						{ label: 'Generics', slug: 'language/generics' },
						{ label: 'Error Handling', slug: 'language/error-handling' },
						{ label: 'Async & Await', slug: 'language/async' },
					],
				},
				{
					label: 'Data & Collections',
					items: [
						{ label: 'Arrays', slug: 'data/arrays' },
						{ label: 'Dicts', slug: 'data/dicts' },
						{ label: 'JSON', slug: 'data/json' },
					],
				},
				{
					label: 'System & Interop',
					items: [
						{ label: 'Sockets', slug: 'system/sockets' },
						{ label: 'FFI (extern)', slug: 'system/ffi' },
						{ label: 'Low-Level I/O', slug: 'system/low-level-io' },
					],
				},
				{
					label: 'Reference',
					items: [
						{ label: 'Types', slug: 'reference/types' },
						{ label: 'Operators', slug: 'reference/operators' },
						{ label: 'Standard Library', slug: 'reference/stdlib' },
						{ label: 'Grammar', slug: 'reference/grammar' },
					],
				},
			],
		}),
	],
});
