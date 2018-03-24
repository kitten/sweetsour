import { join } from 'path';

import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import babel from 'rollup-plugin-babel';
import uglify from 'rollup-plugin-uglify-es';
import es3 from 'rollup-plugin-es3';
import filesize from 'rollup-plugin-filesize';
import alias from 'rollup-plugin-alias';
import { rollup as lernaAliases } from 'lerna-alias';

const pkg = require(join(process.cwd(), './package.json'))

const plugins = [
  commonjs({
    ignoreGlobal: true,
  }),
  nodeResolve({
    modulesOnly: true,
    jsnext: true
  }),
  es3(),
  babel({
    babelrc: false,
    plugins: [
      require.resolve('babel-plugin-closure-elimination'),
      require.resolve('babel-plugin-minify-dead-code-elimination')
    ]
  })
];

const prodPlugins = [
  ...plugins,
  alias(lernaAliases()),
  uglify({
    toplevel: true,
    mangle: {
      toplevel: true
    },
    compress: {
      passes: 2
    }
  }),
  filesize()
];

const withBase = x => Object.assign({}, x, {
  input: './index.js',
  name: pkg.name,
  exports: 'named',
  useStrict: false,
  pureExternalImports: true
});

export default [
  {
    output: [{
      file: 'dist/sweetsour-parser.min.js',
      format: 'umd'
    }],
    plugins: prodPlugins
  }, {
    output: [{
      file: 'dist/sweetsour-parser.js',
      format: 'umd'
    }, {
      file: 'dist/sweetsour-parser.es.js',
      format: 'es'
    }, {
      file: 'dist/sweetsour-parser.cjs.js',
      format: 'cjs'
    }],
    plugins
  }
].map(withBase);
