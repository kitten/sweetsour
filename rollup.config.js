import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import uglify from 'rollup-plugin-uglify-es';

const plugins = [
  nodeResolve(),
  commonjs({
    ignoreGlobal: true,
  })
];

const prodPlugins = [
  ...plugins,
  uglify({
    toplevel: true,
    compress: {
      passes: 2
    }
  })
];

const withBase = x => Object.assign({}, x, {
  input: './lib/es6/src/Main.js',
  name: 'SweetsourParser',
  exports: 'named',
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
      file: 'dist/sweetsour-parser.es.js',
      format: 'es'
    }, {
      file: 'dist/sweetsour-parser.cjs.js',
      format: 'umd'
    }],
    plugins
  }
].map(withBase);
